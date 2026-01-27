%%%-------------------------------------------------------------------
%% @private
%% @doc
%% == NAT scan ==
%% @end
%%%-------------------------------------------------------------------
-module(nat_scan).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start/0
]).

-define(LOG(A, B), io:format("[~p] " ++ A ++ "~n", [?MODULE] ++ B)).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    {ok, Default} = inet:gethostname(),
    FileName = os:getenv("FILE", Default),

    {ok, _} = nat_cache:start([{file, FileName}, {get, false}]),

    %% Setup meck mocks for caching
    ok = meck:new(hackney, [passthrough, no_link]),
    ok = meck:new(gen_udp, [unstick, passthrough, no_link]),
    ok = meck:new(inet_ext, [passthrough, no_link]),

    setup_caching_mocks(),

    _ = natupnp_v1(),
    _ = natupnp_v2(),
    _ = natpmp(),
    _ = natpcp(),

    meck:unload([hackney, gen_udp, inet_ext]),
    nat_cache:stop().

setup_caching_mocks() ->
    %% Hackney caching mock
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

    %% gen_udp caching mock (for NAT-PMP/PCP)
    meck:expect(gen_udp, send,
        fun(Socket, Ip, Port, Msg) ->
            Key = {gen_udp, send, [Ip, Port, Msg]},
            case nat_cache:get(Key) of
                undefined ->
                    meck:passthrough([Socket, Ip, Port, Msg]);
                {_Socket, Ip2, Port2, RespMsg} ->
                    self() ! {udp, Socket, Ip2, Port2, RespMsg},
                    ok
            end
        end),

    %% inet_ext caching mock
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
        end).


natupnp_v1() ->
    ?LOG("[natupnp_v1] discovering", []),
    case natupnp_v1:discover() of
        {ok, Context} ->
            ?LOG("[natupnp_v1] discovered ~p", [Context]),

            case natupnp_v1:add_port_mapping(Context, tcp, 8333, 8333, 3600) of
                {ok, _Since, _InternalPort, _ExternalPort, _MappingLifetime}=OK ->
                    ?LOG("[natupnp_v1] added port mapping ~p", [OK]),
                    case natupnp_v1:delete_port_mapping(Context, tcp, 8333, 8333) of
                        ok ->
                            ?LOG("[natupnp_v1] deleted port mapping", []);
                        {error, R1} ->
                            ?LOG("[natupnp_v1] failed to delete port mapping ~p", [R1])
                    end;
                {error, R1} ->
                    ?LOG("[natupnp_v1] failed to add port mapping ~p", [R1])
            end,

            case natupnp_v1:get_external_address(Context) of
                {ok, ExtAddress} ->
                    ?LOG("[natupnp_v1] got external address ~p", [ExtAddress]);
                {error, R2} ->
                    ?LOG("[natupnp_v1] failed to get external address ~p", [R2])
            end,

            case natupnp_v1:get_internal_address(Context) of
                {ok, IntAddress} ->
                    ?LOG("[natupnp_v1] got internal address ~p", [IntAddress])
            end;
        {error, _Reason} ->
            ?LOG("[natupnp_v1] failed to discover ~p", [_Reason])
    end.

natupnp_v2() ->
    ?LOG("[natupnp_v2] discovering", []),
    case natupnp_v2:discover() of
        {ok, Context} ->
            ?LOG("[natupnp_v2] discovered ~p", [Context]),

            case natupnp_v2:add_port_mapping(Context, tcp, 8333, 8333, 3600) of
                {ok, _Since, _InternalPort, _ExternalPort, _MappingLifetime}=OK ->
                    ?LOG("[natupnp_v2] added port mapping ~p", [OK]),
                    case natupnp_v2:delete_port_mapping(Context, tcp, 8333, 8333) of
                        ok ->
                            ?LOG("[natupnp_v2] deleted port mapping", []);
                        {error, R1} ->
                            ?LOG("[natupnp_v2] failed to delete port mapping ~p", [R1])
                    end;
                {error, R1} ->
                    ?LOG("[natupnp_v2] failed to add port mapping ~p", [R1])
            end,

            case natupnp_v2:get_external_address(Context) of
                {ok, ExtAddress} ->
                    ?LOG("[natupnp_v2] got external address ~p", [ExtAddress]);
                {error, R2} ->
                    ?LOG("[natupnp_v2] failed to get external address ~p", [R2])
            end,

            case natupnp_v2:get_internal_address(Context) of
                {ok, IntAddress} ->
                    ?LOG("[natupnp_v2] got internal address ~p", [IntAddress])
            end;
        {error, _Reason} ->
            ?LOG("[natupnp_v2] failed to discover ~p", [_Reason])
    end.

natpmp() ->
    ?LOG("[natpmp] discovering", []),
    case natpmp:discover() of
        {ok, Context} ->
            ?LOG("[natpmp] discovered ~p", [Context]),

            case natpmp:add_port_mapping(Context, tcp, 8333, 8333, 3600) of
                {ok, _Since, _InternalPort, _ExternalPort, _MappingLifetime}=OK ->
                    ?LOG("[natpmp] added port mapping ~p", [OK]),
                    case natpmp:delete_port_mapping(Context, tcp, 8333, 8333) of
                        ok ->
                            ?LOG("[natpmp] deleted port mapping", []);
                        {error, R1} ->
                            ?LOG("[natpmp] failed to delete port mapping ~p", [R1])
                    end;
                {error, R1} ->
                    ?LOG("[natpmp] failed to add port mapping ~p", [R1])
            end,

            case natpmp:get_external_address(Context) of
                {ok, ExtAddress} ->
                    ?LOG("[natpmp] got external address ~p", [ExtAddress]);
                {error, R2} ->
                    ?LOG("[natpmp] failed to get external address ~p", [R2])
            end,

            case natpmp:get_internal_address(Context) of
                {ok, IntAddress} ->
                    ?LOG("[natpmp] got internal address ~p", [IntAddress])
            end;
        {error, no_nat} ->
            ?LOG("[natpmp] failed to discover not nat", [])
    end.

natpcp() ->
    ?LOG("[natpcp] discovering", []),
    case natpcp:discover() of
        {ok, Context} ->
            ?LOG("[natpcp] discovered ~p", [Context]),

            case natpcp:add_port_mapping(Context, tcp, 8333, 8333, 3600) of
                {ok, _Since, _InternalPort, _ExternalPort, _MappingLifetime}=OK ->
                    ?LOG("[natpcp] added port mapping ~p", [OK]),
                    case natpcp:delete_port_mapping(Context, tcp, 8333, 8333) of
                        ok ->
                            ?LOG("[natpcp] deleted port mapping", []);
                        {error, R1} ->
                            ?LOG("[natpcp] failed to delete port mapping ~p", [R1])
                    end;
                {error, R1} ->
                    ?LOG("[natpcp] failed to add port mapping ~p", [R1])
            end,

            case natpcp:get_external_address(Context) of
                {ok, ExtAddress} ->
                    ?LOG("[natpcp] got external address ~p", [ExtAddress]);
                {error, R2} ->
                    ?LOG("[natpcp] failed to get external address ~p", [R2])
            end,

            case natpcp:get_internal_address(Context) of
                {ok, IntAddress} ->
                    ?LOG("[natpcp] got internal address ~p", [IntAddress])
            end;
        {error, no_nat} ->
            ?LOG("[natpcp] failed to discover no nat", [])
    end.
