%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2024 Benoît Chesneau <benoitc@refuge.io>

%%% @doc NAT Port Mapping Lifecycle Manager
%%%
%%% This gen_server manages NAT port mappings with:
%%% - Automatic discovery and context storage
%%% - Auto-renewal of mappings before expiry
%%% - External IP change detection (multicast for NAT-PMP/PCP, polling for UPnP)
%%% - Event dispatch to registered subscribers

-module(nat_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("nat.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Discover NAT and store context in nat_server
-spec discover() -> ok | no_nat | {error, term()}.
discover() ->
    gen_server:call(?SERVER, discover, 15000).

%% @doc Get stored context (for advanced use)
-spec get_context() -> {ok, term()} | {error, no_context}.
get_context() ->
    gen_server:call(?SERVER, get_context).

%% @doc Add mapping with auto-renewal
-spec add_port_mapping(Protocol, InternalPort, ExternalPort) -> Result when
    Protocol :: tcp | udp,
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Result :: {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(),
               non_neg_integer() | infinity} | {error, term()}.
add_port_mapping(Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

-spec add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) -> Result when
    Protocol :: tcp | udp,
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Lifetime :: non_neg_integer(),
    Result :: {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(),
               non_neg_integer() | infinity} | {error, term()}.
add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) ->
    gen_server:call(?SERVER, {add_mapping, Protocol, InternalPort, ExternalPort, Lifetime}).

%% @doc Delete mapping (cancels renewal timer)
-spec delete_port_mapping(Protocol, InternalPort, ExternalPort) -> ok | {error, term()} when
    Protocol :: tcp | udp,
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer().
delete_port_mapping(Protocol, InternalPort, ExternalPort) ->
    gen_server:call(?SERVER, {delete_mapping, Protocol, InternalPort, ExternalPort}).

%% @doc Get external address
-spec get_external_address() -> {ok, string()} | {error, term()}.
get_external_address() ->
    gen_server:call(?SERVER, get_external_address).

%% @doc Get device address
-spec get_device_address() -> {ok, string()} | {error, term()}.
get_device_address() ->
    gen_server:call(?SERVER, get_device_address).

%% @doc Get internal address
-spec get_internal_address() -> {ok, string()} | {error, term()}.
get_internal_address() ->
    gen_server:call(?SERVER, get_internal_address).

%% @doc List managed mappings
-spec list_mappings() -> [{Protocol, InternalPort, ExternalPort, ExpiresAt}] when
    Protocol :: tcp | udp,
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    ExpiresAt :: non_neg_integer() | infinity.
list_mappings() ->
    gen_server:call(?SERVER, list_mappings).

%% @doc Register pid to receive {nat_event, Event} messages
-spec reg_pid(pid()) -> ok.
reg_pid(Pid) ->
    gen_server:call(?SERVER, {reg_pid, Pid}).

%% @doc Register callback function
-spec reg_fun(fun((term()) -> any())) -> {ok, reference()}.
reg_fun(Fun) ->
    gen_server:call(?SERVER, {reg_fun, Fun}).

%% @doc Unregister pid
-spec unreg_pid(pid()) -> ok.
unreg_pid(Pid) ->
    gen_server:call(?SERVER, {unreg_pid, Pid}).

%% @doc Unregister function by reference
-spec unreg_fun(reference()) -> ok.
unreg_fun(Ref) ->
    gen_server:call(?SERVER, {unreg_fun, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    State = #nat_state{
        nat_ctx = undefined,
        external_ip = undefined,
        last_epoch = undefined,
        mappings = #{},
        reg_pids = #{},
        reg_funs = #{},
        ip_timer = undefined,
        listener_pid = undefined,
        opts = #{}
    },
    {ok, State}.

handle_call(discover, _From, State) ->
    case do_discover(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, no_nat} ->
            {reply, no_nat, State}
    end;

handle_call(get_context, _From, #nat_state{nat_ctx = undefined} = State) ->
    {reply, {error, no_context}, State};
handle_call(get_context, _From, #nat_state{nat_ctx = Ctx} = State) ->
    {reply, {ok, Ctx}, State};

handle_call({add_mapping, Protocol, InternalPort, ExternalPort, Lifetime}, _From, State) ->
    case do_add_mapping(Protocol, InternalPort, ExternalPort, Lifetime, State) of
        {ok, Result, NewState} ->
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete_mapping, Protocol, InternalPort, ExternalPort}, _From, State) ->
    case do_delete_mapping(Protocol, InternalPort, ExternalPort, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_external_address, _From, #nat_state{nat_ctx = undefined} = State) ->
    {reply, {error, no_context}, State};
handle_call(get_external_address, _From, #nat_state{nat_ctx = Ctx} = State) ->
    {Mod, ModCtx} = Ctx,
    {reply, Mod:get_external_address(ModCtx), State};

handle_call(get_device_address, _From, #nat_state{nat_ctx = undefined} = State) ->
    {reply, {error, no_context}, State};
handle_call(get_device_address, _From, #nat_state{nat_ctx = Ctx} = State) ->
    {Mod, ModCtx} = Ctx,
    {reply, Mod:get_device_address(ModCtx), State};

handle_call(get_internal_address, _From, #nat_state{nat_ctx = undefined} = State) ->
    {reply, {error, no_context}, State};
handle_call(get_internal_address, _From, #nat_state{nat_ctx = Ctx} = State) ->
    {Mod, ModCtx} = Ctx,
    {reply, Mod:get_internal_address(ModCtx), State};

handle_call(list_mappings, _From, #nat_state{mappings = Mappings} = State) ->
    List = maps:fold(fun({Protocol, InternalPort}, #mapping{external_port = ExtPort,
                                                            expires_at = ExpiresAt}, Acc) ->
        [{Protocol, InternalPort, ExtPort, ExpiresAt} | Acc]
    end, [], Mappings),
    {reply, List, State};

handle_call({reg_pid, Pid}, _From, #nat_state{reg_pids = Pids} = State) ->
    case maps:is_key(Pid, Pids) of
        true ->
            {reply, ok, State};
        false ->
            MRef = erlang:monitor(process, Pid),
            NewPids = maps:put(Pid, MRef, Pids),
            {reply, ok, State#nat_state{reg_pids = NewPids}}
    end;

handle_call({unreg_pid, Pid}, _From, #nat_state{reg_pids = Pids} = State) ->
    case maps:take(Pid, Pids) of
        {MRef, NewPids} ->
            erlang:demonitor(MRef, [flush]),
            {reply, ok, State#nat_state{reg_pids = NewPids}};
        error ->
            {reply, ok, State}
    end;

handle_call({reg_fun, Fun}, _From, #nat_state{reg_funs = Funs} = State) ->
    Ref = make_ref(),
    NewFuns = maps:put(Ref, Fun, Funs),
    {reply, {ok, Ref}, State#nat_state{reg_funs = NewFuns}};

handle_call({unreg_fun, Ref}, _From, #nat_state{reg_funs = Funs} = State) ->
    NewFuns = maps:remove(Ref, Funs),
    {reply, ok, State#nat_state{reg_funs = NewFuns}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle renewal timer
handle_info({renew_mapping, Key}, State) ->
    NewState = do_renew_mapping(Key, State),
    {noreply, NewState};

%% Handle retry timer after failed renewal
handle_info({retry_renewal, Key}, State) ->
    NewState = do_retry_renewal(Key, State),
    {noreply, NewState};

%% Handle IP poll timer (for UPnP)
handle_info(poll_ip, State) ->
    NewState = do_poll_ip(State),
    {noreply, NewState};

%% Handle NAT-PMP/PCP multicast announcement
handle_info({natpmp_announce, Epoch, ExternalIp}, State) ->
    NewState = handle_multicast_announce(Epoch, ExternalIp, State),
    {noreply, NewState};

%% Handle monitored pid down
handle_info({'DOWN', MRef, process, Pid, _Reason}, #nat_state{reg_pids = Pids} = State) ->
    case maps:take(Pid, Pids) of
        {MRef, NewPids} ->
            {noreply, State#nat_state{reg_pids = NewPids}};
        error ->
            {noreply, State}
    end;

%% Handle multicast listener exit
handle_info({'EXIT', Pid, Reason}, #nat_state{listener_pid = Pid} = State) ->
    logger:warning("NAT multicast listener died: ~p", [Reason]),
    %% Try to restart the listener
    NewState = maybe_start_multicast_listener(State#nat_state{listener_pid = undefined}),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #nat_state{mappings = Mappings, listener_pid = ListenerPid,
                              ip_timer = IpTimer}) ->
    %% Cancel all timers
    maps:foreach(fun(_Key, #mapping{timer_ref = TRef}) ->
        cancel_timer(TRef)
    end, Mappings),
    cancel_timer(IpTimer),
    %% Stop multicast listener
    case ListenerPid of
        undefined -> ok;
        Pid -> exit(Pid, shutdown)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_discover(State) ->
    %% Stop any existing IP monitoring
    State1 = stop_ip_monitoring(State),
    case nat:discover_internal() of
        {ok, Ctx} ->
            %% Get initial external IP
            {Mod, ModCtx} = Ctx,
            ExternalIp = case Mod:get_external_address(ModCtx) of
                {ok, Ip} -> Ip;
                _ -> undefined
            end,
            %% Start IP monitoring based on NAT type
            State2 = State1#nat_state{
                nat_ctx = Ctx,
                external_ip = ExternalIp,
                last_epoch = undefined
            },
            State3 = start_ip_monitoring(State2),
            {ok, State3};
        no_nat ->
            {error, no_nat}
    end.

do_add_mapping(_Protocol, _InternalPort, _ExternalPort, _Lifetime,
               #nat_state{nat_ctx = undefined}) ->
    {error, no_context};
do_add_mapping(Protocol, InternalPort, ExternalPort, Lifetime,
               #nat_state{nat_ctx = Ctx, mappings = Mappings} = State) ->
    {Mod, ModCtx} = Ctx,
    case Mod:add_port_mapping(ModCtx, Protocol, InternalPort, ExternalPort, Lifetime) of
        {ok, Since, IntPort, ExtPort, ActualLifetime} ->
            Key = {Protocol, IntPort},
            %% Cancel existing timer if any
            case maps:get(Key, Mappings, undefined) of
                undefined -> ok;
                #mapping{timer_ref = OldTRef} -> cancel_timer(OldTRef)
            end,
            %% Schedule renewal
            {TRef, ExpiresAt} = schedule_renewal(Key, ActualLifetime),
            Mapping = #mapping{
                key = Key,
                external_port = ExtPort,
                since = Since,
                lifetime = ActualLifetime,
                expires_at = ExpiresAt,
                timer_ref = TRef,
                retry_count = 0
            },
            NewMappings = maps:put(Key, Mapping, Mappings),
            Result = {Since, IntPort, ExtPort, ActualLifetime},
            {ok, Result, State#nat_state{mappings = NewMappings}};
        {error, Reason} ->
            {error, Reason}
    end.

do_delete_mapping(_Protocol, _InternalPort, _ExternalPort,
                  #nat_state{nat_ctx = undefined}) ->
    {error, no_context};
do_delete_mapping(Protocol, InternalPort, ExternalPort,
                  #nat_state{nat_ctx = Ctx, mappings = Mappings} = State) ->
    Key = {Protocol, InternalPort},
    %% Cancel timer if exists
    case maps:get(Key, Mappings, undefined) of
        undefined -> ok;
        #mapping{timer_ref = TRef} -> cancel_timer(TRef)
    end,
    %% Delete from NAT device
    {Mod, ModCtx} = Ctx,
    case Mod:delete_port_mapping(ModCtx, Protocol, InternalPort, ExternalPort) of
        ok ->
            NewMappings = maps:remove(Key, Mappings),
            {ok, State#nat_state{mappings = NewMappings}};
        {error, Reason} ->
            %% Still remove from our tracking even if device delete fails
            NewMappings = maps:remove(Key, Mappings),
            logger:warning("Failed to delete mapping ~p from NAT device: ~p", [Key, Reason]),
            {ok, State#nat_state{mappings = NewMappings}}
    end.

do_renew_mapping(Key, #nat_state{nat_ctx = Ctx, mappings = Mappings} = State) ->
    case maps:get(Key, Mappings, undefined) of
        undefined ->
            State;
        #mapping{external_port = ExtPort, lifetime = Lifetime} = Mapping ->
            {Protocol, InternalPort} = Key,
            {Mod, ModCtx} = Ctx,
            case Mod:add_port_mapping(ModCtx, Protocol, InternalPort, ExtPort, Lifetime) of
                {ok, Since, _IntPort, NewExtPort, NewLifetime} ->
                    %% Renewal successful
                    {TRef, ExpiresAt} = schedule_renewal(Key, NewLifetime),
                    NewMapping = Mapping#mapping{
                        external_port = NewExtPort,
                        since = Since,
                        lifetime = NewLifetime,
                        expires_at = ExpiresAt,
                        timer_ref = TRef,
                        retry_count = 0
                    },
                    NewMappings = maps:put(Key, NewMapping, Mappings),
                    dispatch_event({mapping_renewed, Protocol, InternalPort, NewExtPort, NewLifetime}, State),
                    State#nat_state{mappings = NewMappings};
                {error, Reason} ->
                    %% Renewal failed, schedule retry
                    handle_renewal_failure(Key, Mapping, Reason, State)
            end
    end.

do_retry_renewal(Key, #nat_state{mappings = Mappings} = State) ->
    case maps:get(Key, Mappings, undefined) of
        undefined ->
            State;
        #mapping{retry_count = RetryCount} when RetryCount >= ?MAX_RENEWAL_RETRIES ->
            %% Max retries exceeded, mark as failed
            {Protocol, InternalPort} = Key,
            #mapping{external_port = ExtPort} = maps:get(Key, Mappings),
            dispatch_event({mapping_failed, Protocol, InternalPort, ExtPort, max_retries}, State),
            NewMappings = maps:remove(Key, Mappings),
            State#nat_state{mappings = NewMappings};
        _ ->
            %% Try renewal again
            do_renew_mapping(Key, State)
    end.

handle_renewal_failure(Key, Mapping, Reason, #nat_state{mappings = Mappings} = State) ->
    RetryCount = Mapping#mapping.retry_count + 1,
    if
        RetryCount > ?MAX_RENEWAL_RETRIES ->
            {Protocol, InternalPort} = Key,
            dispatch_event({mapping_failed, Protocol, InternalPort,
                           Mapping#mapping.external_port, Reason}, State),
            NewMappings = maps:remove(Key, Mappings),
            State#nat_state{mappings = NewMappings};
        true ->
            %% Exponential backoff: 5s, 10s, 20s...
            Delay = ?INITIAL_RETRY_DELAY_MS * (1 bsl (RetryCount - 1)),
            TRef = erlang:send_after(Delay, self(), {retry_renewal, Key}),
            NewMapping = Mapping#mapping{
                timer_ref = TRef,
                retry_count = RetryCount
            },
            NewMappings = maps:put(Key, NewMapping, Mappings),
            State#nat_state{mappings = NewMappings}
    end.

schedule_renewal(_Key, Lifetime) when Lifetime =:= infinity ->
    {undefined, infinity};
schedule_renewal(Key, Lifetime) ->
    %% Renew at 50% of lifetime, minimum 60 seconds
    RenewalTime = max(trunc(Lifetime * ?RENEWAL_FACTOR), ?MIN_RENEWAL_INTERVAL),
    TRef = erlang:send_after(RenewalTime * 1000, self(), {renew_mapping, Key}),
    ExpiresAt = erlang:system_time(second) + Lifetime,
    {TRef, ExpiresAt}.

cancel_timer(undefined) -> ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef),
    ok.

%%%===================================================================
%%% IP Monitoring
%%%===================================================================

start_ip_monitoring(#nat_state{nat_ctx = {Mod, _}} = State) when
      Mod =:= natpmp; Mod =:= natpcp ->
    %% NAT-PMP/PCP: Use multicast listener
    maybe_start_multicast_listener(State);
start_ip_monitoring(#nat_state{nat_ctx = {Mod, _}} = State) when
      Mod =:= natupnp_v1; Mod =:= natupnp_v2 ->
    %% UPnP: Use polling
    TRef = erlang:send_after(?IP_POLL_INTERVAL_MS, self(), poll_ip),
    State#nat_state{ip_timer = TRef};
start_ip_monitoring(State) ->
    State.

stop_ip_monitoring(#nat_state{listener_pid = ListenerPid, ip_timer = IpTimer} = State) ->
    %% Stop multicast listener
    case ListenerPid of
        undefined -> ok;
        Pid ->
            exit(Pid, shutdown),
            receive
                {'EXIT', Pid, _} -> ok
            after 1000 -> ok
            end
    end,
    %% Cancel poll timer
    cancel_timer(IpTimer),
    State#nat_state{listener_pid = undefined, ip_timer = undefined}.

maybe_start_multicast_listener(#nat_state{nat_ctx = {Mod, ModCtx}} = State) when
      Mod =:= natpmp; Mod =:= natpcp ->
    %% Get gateway address for the listener
    {ok, Gateway} = Mod:get_device_address(ModCtx),
    Parent = self(),
    Pid = spawn_link(fun() -> multicast_listener_loop(Parent, Gateway) end),
    State#nat_state{listener_pid = Pid};
maybe_start_multicast_listener(State) ->
    State.

multicast_listener_loop(Parent, Gateway) ->
    %% Open UDP socket for multicast
    case gen_udp:open(?NATPMP_MULTICAST_PORT, [
        {reuseaddr, true},
        {ip, ?NATPMP_MULTICAST_ADDR},
        {multicast_ttl, 1},
        {multicast_loop, false},
        {add_membership, {?NATPMP_MULTICAST_ADDR, {0,0,0,0}}},
        binary,
        {active, true}
    ]) of
        {ok, Socket} ->
            multicast_listener_recv(Socket, Parent, Gateway);
        {error, Reason} ->
            logger:warning("Failed to open NAT-PMP multicast socket: ~p", [Reason]),
            %% Fall back to polling
            Parent ! {multicast_failed, Reason},
            ok
    end.

multicast_listener_recv(Socket, Parent, Gateway) ->
    receive
        {udp, Socket, _SrcIp, _SrcPort, Packet} ->
            case parse_multicast_packet(Packet) of
                {ok, Epoch, ExternalIp} ->
                    Parent ! {natpmp_announce, Epoch, ExternalIp};
                {error, _Reason} ->
                    ok
            end,
            multicast_listener_recv(Socket, Parent, Gateway);
        stop ->
            gen_udp:close(Socket),
            ok
    end.

%% Parse NAT-PMP multicast announcement packet
%% Format: version(1), opcode(128), status(2), epoch(4), external_ip(4)
parse_multicast_packet(<<0, 128, 0:16, Epoch:32, A, B, C, D>>) ->
    ExternalIp = inet:ntoa({A, B, C, D}),
    {ok, Epoch, ExternalIp};
parse_multicast_packet(_) ->
    {error, bad_packet}.

handle_multicast_announce(Epoch, NewExternalIp,
                          #nat_state{external_ip = OldExternalIp,
                                     last_epoch = LastEpoch} = State) ->
    %% Check for IP change via epoch reset or actual IP difference
    IpChanged = (OldExternalIp =/= undefined andalso
                 OldExternalIp =/= NewExternalIp),
    EpochReset = (LastEpoch =/= undefined andalso Epoch < LastEpoch),

    NewState = State#nat_state{
        external_ip = NewExternalIp,
        last_epoch = Epoch
    },

    if
        IpChanged orelse EpochReset ->
            dispatch_event({ip_changed, OldExternalIp, NewExternalIp}, NewState),
            %% Re-establish all mappings with new epoch
            renew_all_mappings(NewState);
        true ->
            NewState
    end.

do_poll_ip(#nat_state{nat_ctx = Ctx, external_ip = OldIp} = State) ->
    {Mod, ModCtx} = Ctx,
    case Mod:get_external_address(ModCtx) of
        {ok, NewIp} when NewIp =/= OldIp, OldIp =/= undefined ->
            dispatch_event({ip_changed, OldIp, NewIp}, State),
            %% Schedule next poll
            TRef = erlang:send_after(?IP_POLL_INTERVAL_MS, self(), poll_ip),
            renew_all_mappings(State#nat_state{external_ip = NewIp, ip_timer = TRef});
        {ok, NewIp} ->
            %% No change, schedule next poll
            TRef = erlang:send_after(?IP_POLL_INTERVAL_MS, self(), poll_ip),
            State#nat_state{external_ip = NewIp, ip_timer = TRef};
        {error, Reason} ->
            logger:warning("Failed to poll external IP: ~p", [Reason]),
            %% Dispatch context_lost event
            dispatch_event({context_lost, Reason}, State),
            %% Schedule next poll anyway
            TRef = erlang:send_after(?IP_POLL_INTERVAL_MS, self(), poll_ip),
            State#nat_state{ip_timer = TRef}
    end.

renew_all_mappings(#nat_state{mappings = Mappings} = State) ->
    %% Cancel all existing timers and trigger immediate renewal
    maps:fold(fun(Key, #mapping{timer_ref = TRef}, AccState) ->
        cancel_timer(TRef),
        do_renew_mapping(Key, AccState)
    end, State, Mappings).

%%%===================================================================
%%% Event Dispatch
%%%===================================================================

dispatch_event(Event, #nat_state{reg_pids = Pids, reg_funs = Funs}) ->
    %% Send to registered pids
    maps:foreach(fun(Pid, _MRef) ->
        Pid ! {nat_event, Event}
    end, Pids),
    %% Call registered functions (spawn to avoid blocking)
    maps:foreach(fun(_Ref, Fun) ->
        spawn(fun() ->
            try
                Fun(Event)
            catch
                Class:Reason:Stack ->
                    logger:error("NAT event handler crashed: ~p:~p~n~p",
                                [Class, Reason, Stack])
            end
        end)
    end, Funs),
    ok.
