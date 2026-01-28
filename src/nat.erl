%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2024 Benoît Chesneau <benoitc@refuge.io>

-module(nat).

%% Main API (uses nat_server)
-export([discover/0]).
-export([get_context/0]).
-export([add_port_mapping/3, add_port_mapping/4]).
-export([delete_port_mapping/3]).
-export([get_external_address/0]).
-export([get_device_address/0]).
-export([get_internal_address/0]).
-export([list_mappings/0]).
-export([reg_pid/1, unreg_pid/1]).
-export([reg_fun/1, unreg_fun/1]).

%% Internal API (for nat_server)
-export([discover_internal/0]).

%% Debug API
-export([debug_start/1]).
-export([debug_stop/0]).

-include("nat.hrl").

-define(BACKENDS, [natupnp_v1, natupnp_v2, natpmp, natpcp]).
-define(DISCOVER_TIMEOUT, 10000).

-type nat_ctx() :: {module(), term()}.
-type nat_protocol() :: tcp | udp.

-export_type([nat_ctx/0, nat_protocol/0]).

%%%===================================================================
%%% Debug API
%%%===================================================================

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

%%%===================================================================
%%% Main API
%%%===================================================================

-spec discover() -> ok | no_nat | {error, term()}.
%% @doc Discover a NAT gateway and store the context in nat_server.
discover() ->
    nat_server:discover().

-spec get_context() -> {ok, nat_ctx()} | {error, no_context}.
%% @doc Get the stored NAT context.
get_context() ->
    nat_server:get_context().

-spec add_port_mapping(Protocol, InternalPort, ExternalPort) -> Result when
    Protocol :: nat_protocol(),
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Result :: {ok, Since :: non_neg_integer(), InternalPort :: non_neg_integer(),
               ExternalPort :: non_neg_integer(), Lifetime :: non_neg_integer() | infinity} |
              {error, term()}.
%% @doc Add a port mapping with default lifetime (auto-renewed).
add_port_mapping(Protocol, InternalPort, ExternalPort) ->
    nat_server:add_port_mapping(Protocol, InternalPort, ExternalPort).

-spec add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) -> Result when
    Protocol :: nat_protocol(),
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Lifetime :: non_neg_integer(),
    Result :: {ok, Since :: non_neg_integer(), InternalPort :: non_neg_integer(),
               ExternalPort :: non_neg_integer(), ActualLifetime :: non_neg_integer() | infinity} |
              {error, term()}.
%% @doc Add a port mapping with specified lifetime (auto-renewed).
add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) ->
    nat_server:add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime).

-spec delete_port_mapping(Protocol, InternalPort, ExternalPort) -> ok | {error, term()} when
    Protocol :: nat_protocol(),
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer().
%% @doc Delete a port mapping (cancels auto-renewal timer).
delete_port_mapping(Protocol, InternalPort, ExternalPort) ->
    nat_server:delete_port_mapping(Protocol, InternalPort, ExternalPort).

-spec get_external_address() -> {ok, string()} | {error, term()}.
%% @doc Get the external (public) IP address.
get_external_address() ->
    nat_server:get_external_address().

-spec get_device_address() -> {ok, string()} | {error, term()}.
%% @doc Get the NAT gateway's IP address.
get_device_address() ->
    nat_server:get_device_address().

-spec get_internal_address() -> {ok, string()} | {error, term()}.
%% @doc Get the local device's IP address.
get_internal_address() ->
    nat_server:get_internal_address().

-spec list_mappings() -> [{Protocol, InternalPort, ExternalPort, ExpiresAt}] when
    Protocol :: tcp | udp,
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    ExpiresAt :: non_neg_integer() | infinity.
%% @doc List all managed port mappings.
list_mappings() ->
    nat_server:list_mappings().

-spec reg_pid(pid()) -> ok.
%% @doc Register pid to receive {nat_event, Event} messages.
reg_pid(Pid) ->
    nat_server:reg_pid(Pid).

-spec unreg_pid(pid()) -> ok.
%% @doc Unregister pid from receiving events.
unreg_pid(Pid) ->
    nat_server:unreg_pid(Pid).

-spec reg_fun(fun((Event :: term()) -> any())) -> {ok, reference()}.
%% @doc Register callback function for events.
reg_fun(Fun) ->
    nat_server:reg_fun(Fun).

-spec unreg_fun(reference()) -> ok.
%% @doc Unregister callback function by reference.
unreg_fun(Ref) ->
    nat_server:unreg_fun(Ref).

%%%===================================================================
%%% Internal API
%%%===================================================================

-spec discover_internal() -> {ok, nat_ctx()} | no_nat.
%% @doc Internal discovery function used by nat_server.
discover_internal() ->
    Self = self(),
    Ref = make_ref(),
    Workers = spawn_workers(?BACKENDS, Self, Ref, []),
    discover_loop(Workers, Ref).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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
spawn_workers([Backend | Rest], Parent, Ref, Acc) ->
    Pid = spawn_link(fun() -> discover_worker(Backend, Parent, Ref) end),
    monitor_worker(Pid),
    spawn_workers(Rest, Parent, Ref, [Pid | Acc]).

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
kill_workers([Pid | Rest]) ->
    catch unlink(Pid),
    catch exit(Pid, shutdown),
    receive
        {'DOWN', _, _, Pid, _} ->
            demonitor_worker(Pid),
            ok
    end,
    kill_workers(Rest).
