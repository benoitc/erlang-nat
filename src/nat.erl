%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016 Benoît Chesneau <benoitc@refuge.io>

-module(nat).

-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).

-include("nat.hrl").

-define(BACKENDS, [natupnp_v1, natupnp_v2, natpmp]).
-define(DISCOVER_TIMEOUT, 10000).

-type nat_ctx() :: any().
-type nat_protocol() :: tcp | udp.

-export_type([nat_ctx/0,
              nat_protocol/0]).


-spec discover() -> {ok, NatCtx} | no_nat when
      NatCtx :: nat_ctx().
%% @doc discover a NAT gateway and return a context that can be used with
%% othe functions.
discover() ->
    _ = application:start(inets),
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
      MappingLifetime :: non_neg_integer(),
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
      MappingLifetime :: non_neg_integer(),
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
