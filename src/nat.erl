%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

-module(nat).

-behaviour(gen_server).

%% API
-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([maintain_port_mapping/4]).
-export([delete_port_mapping/4]).


%% Debug API
-export([get_httpc_profile/0]).
-export([debug_start/1]).
-export([debug_stop/0]).

%% gen_server API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("nat.hrl").

-type nat_ctx() :: any().
-type nat_protocol() :: tcp | udp.

-export_type([nat_ctx/0,
              nat_protocol/0]).

-define(BACKENDS, [natupnp_v1, natupnp_v2, natpmp]).
-define(DISCOVER_TIMEOUT, 10000).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec discover() -> {ok, NatCtx} | no_nat when
      NatCtx :: nat_ctx().
%% @doc discover a NAT gateway and return a context that can be used with
%% other functions.
discover() ->
    gen_server:call(?SERVER, discover, 50000).

-spec get_device_address(NatCtx) -> {ok, DeviceIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      DeviceIp :: string(),
      Reason :: any().
%% @doc get the IP address of the gateway.
get_device_address({Mod, Ctx}) ->
    gen_server:call(?SERVER, {get_device_address, Mod, Ctx}, 50000).

-spec get_external_address(NatCtx) -> {ok, ExternalIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      ExternalIp :: string(),
      Reason :: any().
%% @doc return the external address of the gateway device
get_external_address({Mod, Ctx}) ->
    gen_server:call(?SERVER, {get_external_address, Mod, Ctx}, 50000).

-spec get_internal_address(NatCtx) -> {ok, InternalIp} | {error, Reason} when
      NatCtx :: nat_ctx(),
      InternalIp :: string(),
      Reason :: any().
%% @doc return the address address of the local device
get_internal_address({Mod, Ctx}) ->
    gen_server:call(?SERVER, {get_internal_address, Mod, Ctx}, 50000).

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
    gen_server:call(?SERVER,
                    {add_port_mapping, NatCtx, Protocol, InternalPort, ExternalPort, ?RECOMMENDED_MAPPING_LIFETIME_SECONDS}, 50000).

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
add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPort, Lifetime) ->
    gen_server:call(?SERVER,
                    {add_port_mapping, NatCtx, Protocol, InternalPort, ExternalPort, Lifetime}, 50000).

-spec maintain_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) ->
                                   {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
                                       when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      MappingLifetime :: non_neg_integer() | infinity,
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      Reason :: any() | timeout.
%% @doc maintain a port mapping
maintain_port_mapping(NatCtx, Protocol, InternalPort, ExternalPort) ->
    gen_server:call(?SERVER,
                    {maintain_port_mapping, NatCtx, Protocol, InternalPort, ExternalPort}, 50000).

-spec delete_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) ->
                                 ok | {error, Reason}
                                     when
      NatCtx :: nat_ctx(),
      Protocol :: nat_protocol(),
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Reason :: any() | timeout.
%% @doc delete a port mapping
delete_port_mapping(NatCtx, Protocol, InternalPort, ExternalPort) ->
    gen_server:call(?SERVER,
                    {delete_port_mapping, NatCtx, Protocol, InternalPort, ExternalPort}, 50000).

%%%===================================================================
%%% Debug API
%%%===================================================================

-spec get_httpc_profile() -> pid().
get_httpc_profile() ->
    gen_server:call(?SERVER, get_httpc_profile).

-spec debug_start(string()) -> ok.
debug_start(File) ->
    {ok, _} = nat_cache:start([{file, File}]),
    ok = intercept:add(gen_udp, gen_udp_intercepts, [{{send, 4}, send}]),
    ok = intercept:add(httpc, httpc_intercepts, [{{request, 1}, request}, {{request, 4}, request}]),
    ok = intercept:add(inet_ext, inet_ext_intercepts, [{{get_internal_address, 1}, get_internal_address}]).

-spec debug_stop() -> ok.
debug_stop() ->
    ok = intercept:clean(gen_udp),
    ok = intercept:clean(httpc),
    ok = intercept:clean(inet_ext),
    ok = nat_cache:stop().

%%%===================================================================
%%% Gen server API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    error_logger:info_msg("Starting UPnP/NAT-PMP service"),
    _ = rand_compat:seed(erlang:phash2([node()]),
                         erlang:monotonic_time(),
                         erlang:unique_integer()),
    {ok, Profile} = inets:start(httpc, [{profile, nat}], stand_alone),
    State = #state{httpc_profile = Profile},
    erlang:send_after(rand:uniform(1000), self(), renew_port_mappings),
    {ok, State}.

handle_call(discover, _From, #state{httpc_profile = HttpcProfile} = State) ->
    Result = do_discover(HttpcProfile),
    {reply, Result, State};
handle_call({get_device_address, Mod, Ctx}, _From, State) ->
    {reply, Mod:get_device_address(Ctx), State};
handle_call({get_external_address, Mod, Ctx}, _From, #state{httpc_profile = HttpcProfile} = State) ->
    {reply, Mod:get_external_address(Ctx, HttpcProfile), State};
handle_call({get_internal_address, Mod, Ctx}, _From, State) ->
    {reply, Mod:get_internal_address(Ctx), State};
handle_call({add_port_mapping, {Mod, Ctx}, Protocol, InternalPort, ExternalPort, Lifetime},
            _From, #state{httpc_profile = HttpcProfile} = State) ->
    {reply, Mod:add_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, Lifetime, HttpcProfile), State};
handle_call({maintain_port_mapping, {Mod, Ctx}, Protocol, InternalPort, ExternalPort},
            _From, #state{httpc_profile = HttpcProfile, mappings = Mappings0} = State0) ->
    Response = Mod:add_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, ?RECOMMENDED_MAPPING_LIFETIME_SECONDS, HttpcProfile),
    case Response of
        {ok, _Since, InternalPort, ExternalPort, _MappingLifetime} ->
            State = #state{mappings = [{Protocol, InternalPort, ExternalPort} | Mappings0]},
            {reply, Response, State};
        {error, _Reason} = Error->
            {reply, Error, State0}
    end;
handle_call({delete_port_mapping, {Mod, Ctx}, Protocol, InternalPort, ExternalPort},
            _From, #state{httpc_profile = HttpcProfile, mappings = Mappings0} = State0) ->
    State = State0#state{mappings = lists:delete({Protocol, InternalPort, ExternalPort}, Mappings0)},
    {reply, Mod:delete_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, HttpcProfile), State};
handle_call(get_httpc_profile, _From, #state{httpc_profile = HttpcProfile} = State) ->
    {reply, HttpcProfile, State};
handle_call(Request, _From, State) ->
    error_logger:warning_msg("Received unknown request: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Other, State) ->
    error_logger:warning_msg("Received unknown cast: ~p", [Other]),
    {noreply, State}.

handle_info(renew_port_mappings, #state{mappings = []} = State) ->
    %% Give additional 10 secs for UPnP/NAT-PMP discovery and setup, to
    %% make sure there is continuity in port mapping.
    erlang:send_after(1000 * (?RECOMMENDED_MAPPING_LIFETIME_SECONDS - 10), self(), renew_port_mappings),
    {noreply, State};
handle_info(renew_port_mappings, #state{mappings = Mappings, httpc_profile = HttpcProfile} = State) ->
    case do_discover(HttpcProfile) of
        {ok, {Mod, Ctx}} ->
            lists:foreach(
              fun({Protocol, InternalPort, ExternalPort}) ->
                      case Mod:add_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, ?RECOMMENDED_MAPPING_LIFETIME_SECONDS, HttpcProfile) of
                          {ok, _Since, _InternalPort, _ExternalPort, _MappingLifetime} ->
                              ok;
                          {error, _Reason} = Error ->
                              error_logger:warning_msg("UPnP/NAT-PMP mapping renewal between ~p and ~p failed: ~p",
                                                       [InternalPort, ExternalPort, Error])
                      end
              end, Mappings);
        no_nat ->
            error_logger:warning_msg("UPnP/NAT-PMP discovery failed during lease renewal")
    end,
    %% Give additional 10 secs for UPnP/NAT-PMP discovery and setup, to
    %% make sure there is continuity in port mapping.
    erlang:send_after(1000 * (?RECOMMENDED_MAPPING_LIFETIME_SECONDS - 10), self(), renew_port_mappings),
    {noreply, State};
handle_info(Other, State) ->
    error_logger:warning_msg("Received unknown info message: ~p", [Other]),
    {noreply, State}.

terminate(_Reason, #state{httpc_profile = HttpcProfile, mappings = Mappings}) when is_pid(HttpcProfile) ->
    case do_discover(HttpcProfile) of
        {ok, {Mod, Ctx}} ->
            lists:foreach(
              fun({Protocol, InternalPort, ExternalPort}) ->
                      case Mod:delete_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, HttpcProfile) of
                          ok -> ok;
                          {error, _Reason} = Error ->
                              error_logger:warning_msg("UPnP/NAT-PMP mapping removal between ~p and ~p failed: ~p",
                                                       [InternalPort, ExternalPort, Error])
                      end
              end, Mappings);
        no_nat ->
            error_logger:warning_msg("UPnP/NAT-PMP discovery failed during mappings removal")
    end,
    gen_server:stop(HttpcProfile, normal, infinity),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_discover(HttpcProfile) ->
    Self = self(),
    Ref = make_ref(),
    Workers = spawn_workers(?BACKENDS, HttpcProfile, Self, Ref, []),
    discover_loop(Workers, Ref).

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

discover_worker(Backend, HttpcProfile, Parent, Ref) ->
    case Backend:discover(HttpcProfile) of
        {ok, Ctx} ->
            Parent ! {nat, Ref, self(), {Backend, Ctx}};
        _Error ->
            ok
    end.

spawn_workers([], _HttpcProfile, _Parent, _Ref, Workers) ->
    Workers;
spawn_workers([Backend | Rest], HttpcProfile, Parent, Ref, Acc) ->
    Pid = spawn_link(fun() -> discover_worker(Backend, HttpcProfile, Parent, Ref) end),
    monitor_worker(Pid),
    spawn_workers(Rest, HttpcProfile, Parent, Ref, [Pid | Acc]).

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
