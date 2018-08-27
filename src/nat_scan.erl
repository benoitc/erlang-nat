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
    ok = intercept:add(gen_udp, gen_udp_intercepts, [{{send, 4}, send}]),
    ok = intercept:add(httpc, httpc_intercepts, [{{request, 1}, request}, {{request, 4}, request}]),
    ok = intercept:add(inet_ext, inet_ext_intercepts, [{{get_internal_address, 1}, get_internal_address}]),

    _ = natupnp_v1(),
    _ = natupnp_v2(),
    _ = natpmp(),

    ok = intercept:clean(gen_udp),
    ok = intercept:clean(httpc),
    ok = intercept:clean(inet_ext),
    nat_cache:stop().


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
        timeout ->
            ?LOG("[natupnp_v1] failed to discover timeout", []);
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
        timeout ->
            ?LOG("[natupnp_v2] failed to discover timeout", []);
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
        no_nat ->
            ?LOG("[natpmp] failed to discover not nat", [])
    end.
