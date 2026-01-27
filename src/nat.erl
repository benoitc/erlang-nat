%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

-module(nat).

-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).
-export([delete_all_mappings/2]).
-export([list_port_mappings/1, list_port_mappings/2]).
-export([get_port_mapping_count/1]).
-export([debug_start/1]).
-export([debug_stop/0]).

-include("nat.hrl").

-define(BACKENDS, [natupnp_v1, natupnp_v2, natpmp, natpcp]).
-define(DISCOVER_TIMEOUT, 10000).

-type nat_ctx() :: any().
-type nat_protocol() :: tcp | udp.

-export_type([nat_ctx/0,
              nat_protocol/0,
              nat_upnp/0]).

-type nat_upnp() :: #nat_upnp{}.

-spec debug_start(string()) -> ok.
debug_start(File) ->
    {ok, _} = nat_cache:start([{file, File}]),
    setup_mocks().

-spec debug_stop() -> ok.
debug_stop() ->
    teardown_mocks(),
    ok = nat_cache:stop().

%% Mock setup using meck for testing
setup_mocks() ->
    ok = meck:new(hackney, [passthrough, no_link]),
    ok = meck:new(gen_udp, [unstick, passthrough, no_link]),
    ok = meck:new(inet_ext, [passthrough, no_link]),

    %% Mock hackney requests
    meck:expect(hackney, request,
        fun(Method, Url, Headers, Body, Opts) ->
            Key = {hackney, request, [Method, Url]},
            case nat_cache:get(Key) of
                undefined ->
                    Result = meck:passthrough([Method, Url, Headers, Body, Opts]),
                    nat_cache:put(Key, Result),
                    Result;
                Cached ->
                    Cached
            end
        end),

    %% Mock gen_udp send
    meck:expect(gen_udp, send,
        fun(Socket, Ip, Port, Msg) ->
            Key = {gen_udp, send, [Ip, Port, Msg]},
            case nat_cache:get(Key) of
                {_Socket, Ip2, Port2, RespMsg} ->
                    self() ! {udp, Socket, Ip2, Port2, RespMsg},
                    ok;
                undefined ->
                    meck:passthrough([Socket, Ip, Port, Msg])
            end
        end),

    %% Mock inet_ext get_internal_address
    meck:expect(inet_ext, get_internal_address,
        fun(Gateway) ->
            Key = {inet_ext, get_internal_address, [Gateway]},
            case nat_cache:get(Key) of
                undefined ->
                    Result = meck:passthrough([Gateway]),
                    nat_cache:put(Key, Result),
                    Result;
                Cached ->
                    Cached
            end
        end),
    ok.

teardown_mocks() ->
    catch meck:unload(hackney),
    catch meck:unload(gen_udp),
    catch meck:unload(inet_ext),
    ok.

-spec discover() -> {ok, NatCtx} | no_nat when
      NatCtx :: nat_ctx().
%% @doc discover a NAT gateway and return a context that can be used with
%% other functions.
discover() ->
    Self = self(),
    Ref = make_ref(),
    Workers = spawn_workers(?BACKENDS, Self, Ref, []),
    discover_loop(Workers, Ref).

-spec get_device_address(NatCtx) -> {ok, DeviceIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      DeviceIp :: string(),
      Reason :: any().
%% @doc get the IP address of the gateway.
get_device_address({Mod, Ctx}) ->
    Mod:get_device_address(Ctx).

-spec get_external_address(NatCtx) -> {ok, ExternalIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      ExternalIp :: string(),
      Reason :: any().
%% @doc return the external address of the gateway device
get_external_address({Mod, Ctx}) ->
    Mod:get_external_address(Ctx).

-spec get_internal_address(NatCtx) -> {ok, InternalIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      InternalIp :: string(),
      Reason :: any().
%% @doc return the address address of the local device
get_internal_address({Mod, Ctx}) ->
    Mod:get_internal_address(Ctx).


-spec add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer() | infinity,
      Reason :: any() | timeout.
%% @doc add a port mapping with default lifetime
add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

-spec add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest, Lifetime) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Lifetime :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer() | infinity,
      Reason :: any() | timeout().
%% @doc add a port mapping
add_port_mapping({Mod, Ctx}, Protocol, InternalPort, ExternalPort, Lifetime) ->
    Mod:add_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, Lifetime).


-spec delete_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) ->
    ok | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Reason :: any() | timeout.
%% @doc delete a port mapping
delete_port_mapping({Mod, Ctx}, Protocol, InternalPort, ExternalPort) ->
    Mod:delete_port_mapping(Ctx, Protocol, InternalPort, ExternalPort).


-spec delete_all_mappings(NatCtx, Protocol) ->
    ok | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      Reason :: not_supported | any().
%% @doc delete all port mappings for a protocol (NAT-PMP only)
delete_all_mappings({natpmp, Ctx}, Protocol) ->
    natpmp:delete_all_mappings(Ctx, Protocol);
delete_all_mappings({_, _}, _Protocol) ->
    {error, not_supported}.  %% UPnP doesn't have bulk delete


-spec list_port_mappings(NatCtx) ->
    {ok, [#port_mapping{}]} | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Reason :: not_supported | any().
%% @doc list all port mappings (UPnP only)
list_port_mappings(NatCtx) ->
    list_port_mappings(NatCtx, 100).

-spec list_port_mappings(NatCtx, Limit) ->
    {ok, [#port_mapping{}]} | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Limit :: non_neg_integer(),
      Reason :: not_supported | any().
%% @doc list all port mappings up to Limit (UPnP only)
list_port_mappings({natupnp_v1, Ctx}, Limit) ->
    natupnp_v1:list_port_mappings(Ctx, Limit);
list_port_mappings({natupnp_v2, Ctx}, Limit) ->
    natupnp_v2:list_port_mappings(Ctx, Limit);
list_port_mappings({natpmp, _}, _Limit) ->
    {error, not_supported};
list_port_mappings({natpcp, _}, _Limit) ->
    {error, not_supported}.


-spec get_port_mapping_count(NatCtx) ->
    {ok, Count} | {error, Reason}
      when
      NatCtx :: nat_ctx(),
      Count :: non_neg_integer(),
      Reason :: not_supported | any().
%% @doc get the total number of port mappings (UPnP only)
get_port_mapping_count({natupnp_v1, Ctx}) ->
    natupnp_v1:get_port_mapping_count(Ctx);
get_port_mapping_count({natupnp_v2, Ctx}) ->
    natupnp_v2:get_port_mapping_count(Ctx);
get_port_mapping_count({natpmp, _}) ->
    {error, not_supported};
get_port_mapping_count({natpcp, _}) ->
    {error, not_supported}.


%% internals
discover_loop([], _Ref) ->
    no_nat;
discover_loop(Workers, Ref) ->
    receive
        {nat, Ref, Pid, Ctx} ->
            demonitor_worker(Pid),
            kill_workers(Workers -- [Pid]),
            {ok, Ctx};
        {'DOWN', _, _, Pid, _} ->
            demonitor_worker(Pid),
            discover_loop(Workers -- [Pid], Ref)
    after
        ?DISCOVER_TIMEOUT ->
            kill_workers(Workers),
            no_nat
    end.


discover_worker(Backend, Parent, Ref) ->
    case Backend:discover() of
        {ok, Ctx} ->
            Parent ! {nat, Ref, self(), {Backend, Ctx}};
        _Error ->
            ok
    end.

spawn_workers([], _Parent, _Ref, Workers) ->
    Workers;
spawn_workers([Backend | Rest], Parent, Ref, Acc) ->
    Pid = spawn_link(fun() -> discover_worker(Backend, Parent, Ref) end),
    monitor_worker(Pid),
    spawn_workers(Rest, Parent, Ref, [Pid | Acc]).

monitor_worker(Pid) ->
    MRef = erlang:monitor(process, Pid),
    _ = erlang:put({discover, Pid}, MRef).

demonitor_worker(Pid) ->
    case erlang:erase({discover, Pid}) of
        undefined -> ok;
        MRef -> erlang:demonitor(MRef, [flush])
    end,
    ok.

kill_workers([]) -> ok;
kill_workers([Pid | Rest]) ->
    catch unlink(Pid),
    catch exit(Pid, shutdown),
    receive
        {'DOWN', _, _, Pid, _} ->
            demonitor_worker(Pid),
            ok
    end,
    kill_workers(Rest).
