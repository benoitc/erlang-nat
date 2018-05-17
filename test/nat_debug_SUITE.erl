-module(nat_debug_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0
    ,init_per_suite/1
    ,end_per_suite/1
]).

-export([
    natpmp/1
    ,natupnp_v1/1
    ,natupnp_v2/1
]).

-define(SCAN_FILE, "nat_scan").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Running tests for this suite
%% @end
%%--------------------------------------------------------------------
all() ->
    [natpmp, natupnp_v1, natupnp_v2].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special init config for suite
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ct:pal("START OF ~p", [?MODULE]),
    true = os:putenv("FILE", ?SCAN_FILE),
    ok = nat_scan:start(),
    Config.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%   Special end config for groups
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ct:pal("END OF ~p", [?MODULE]),
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
natpmp(_Config) ->
    Discover = natpmp:discover(),
    nat:debug_start(?SCAN_FILE),
    ?assertEqual(Discover, natpmp:discover()),
    nat:debug_stop(),

    case Discover of
        {ok, Context} ->
            AddPortMapping = natpmp:add_port_mapping(Context, tcp, 8333, 8333, 3600),
            nat:debug_start(?SCAN_FILE),
            case AddPortMapping of
                {ok, _Since, InternalPort, ExternalPortRequest, MappingLifetime} ->
                    ?assertMatch(
                        {ok, _, InternalPort, ExternalPortRequest, MappingLifetime}
                        ,natpmp:add_port_mapping(Context, tcp, 8333, 8333, 3600)
                    );
                Any ->
                    ?assertEqual(Any, natpmp:add_port_mapping(Context, tcp, 8333, 8333, 3600))
            end,
            nat:debug_stop(),

            GetExtAddress = natpmp:get_external_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetExtAddress, natpmp:get_external_address(Context)),
            nat:debug_stop(),

            GetIntAddress = natpmp:get_internal_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetIntAddress, natpmp:get_internal_address(Context)),
            nat:debug_stop();
        no_nat -> ok;
        {error, _Reason} -> ok
    end.

natupnp_v1(_Config) ->
    Discover = natupnp_v1:discover(),
    nat:debug_start(?SCAN_FILE),
    ?assertEqual(Discover, natupnp_v1:discover()),
    nat:debug_stop(),

    case Discover of
        {ok, Context} ->
            AddPortMapping = natupnp_v1:add_port_mapping(Context, tcp, 8333, 8333, 3600),
            nat:debug_start(?SCAN_FILE),
            case AddPortMapping of
                {ok, _Since, InternalPort, ExternalPortRequest, MappingLifetime} ->
                    ?assertMatch(
                        {ok, _, InternalPort, ExternalPortRequest, MappingLifetime}
                        ,natupnp_v1:add_port_mapping(Context, tcp, 8333, 8333, 3600)
                    );
                Any ->
                    ?assertEqual(Any, natupnp_v1:add_port_mapping(Context, tcp, 8333, 8333, 3600))
            end,
            nat:debug_stop(),

            GetExtAddress = natupnp_v1:get_external_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetExtAddress, natupnp_v1:get_external_address(Context)),
            nat:debug_stop(),

            GetIntAddress = natupnp_v1:get_internal_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetIntAddress, natupnp_v1:get_internal_address(Context)),
            nat:debug_stop();
        timeout -> ok;
        {error, _Reason} -> ok
    end.

natupnp_v2(_Config) ->
    Discover = natupnp_v2:discover(),
    nat:debug_start(?SCAN_FILE),
    ?assertEqual(Discover, natupnp_v2:discover()),
    nat:debug_stop(),

    case Discover of
        {ok, Context} ->
            AddPortMapping = natupnp_v2:add_port_mapping(Context, tcp, 8333, 8333, 3600),
            nat:debug_start(?SCAN_FILE),
            case AddPortMapping of
                {ok, _Since, InternalPort, ExternalPortRequest, MappingLifetime} ->
                    ?assertMatch(
                        {ok, _, InternalPort, ExternalPortRequest, MappingLifetime}
                        ,natupnp_v2:add_port_mapping(Context, tcp, 8333, 8333, 3600)
                    );
                Any ->
                    ?assertEqual(Any, natupnp_v2:add_port_mapping(Context, tcp, 8333, 8333, 3600))
            end,
            nat:debug_stop(),

            GetExtAddress = natupnp_v2:get_external_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetExtAddress, natupnp_v2:get_external_address(Context)),
            nat:debug_stop(),

            GetIntAddress = natupnp_v2:get_internal_address(Context),
            nat:debug_start(?SCAN_FILE),
            ?assertEqual(GetIntAddress, natupnp_v2:get_internal_address(Context)),
            nat:debug_stop();
        timeout -> ok;
        {error, _Reason} -> ok
    end.
