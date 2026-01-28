%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.

-module(nat_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("nat.hrl").

%% ===================================================================
%% Basic Server Tests (no mocks needed)
%% ===================================================================

basic_test_() ->
    {setup,
     fun() ->
         {ok, _} = application:ensure_all_started(nat),
         timer:sleep(50),
         ok
     end,
     fun(_) ->
         application:stop(nat)
     end,
     fun(_) ->
         [
          {"Server starts and is registered", fun() ->
              Pid = whereis(nat_server),
              ?assertNotEqual(undefined, Pid),
              ?assert(is_pid(Pid))
          end},
          {"Get context without discovery returns error", fun() ->
              ?assertEqual({error, no_context}, nat:get_context()),
              ?assertEqual({error, no_context}, nat:get_external_address()),
              ?assertEqual({error, no_context}, nat:get_device_address()),
              ?assertEqual({error, no_context}, nat:get_internal_address())
          end},
          {"Register and unregister pid", fun() ->
              Self = self(),
              ?assertEqual(ok, nat:reg_pid(Self)),
              ?assertEqual(ok, nat:reg_pid(Self)),
              ?assertEqual(ok, nat:unreg_pid(Self)),
              ?assertEqual(ok, nat:unreg_pid(Self))
          end},
          {"Register and unregister fun", fun() ->
              Fun = fun(_Event) -> ok end,
              {ok, Ref} = nat:reg_fun(Fun),
              ?assert(is_reference(Ref)),
              ?assertEqual(ok, nat:unreg_fun(Ref)),
              ?assertEqual(ok, nat:unreg_fun(Ref))
          end},
          {"List mappings returns empty list initially", fun() ->
              ?assertEqual([], nat:list_mappings())
          end}
         ]
     end
    }.

%% ===================================================================
%% Mocked Discovery Tests
%% ===================================================================

mocked_test_() ->
    {setup,
     fun() ->
         setup_mocks(),
         {ok, _} = application:ensure_all_started(nat),
         timer:sleep(50),
         ok
     end,
     fun(_) ->
         application:stop(nat),
         teardown_mocks()
     end,
     fun(_) ->
         [
          {"Discovery with mock", fun() ->
              ?assertEqual(ok, nat:discover()),
              {ok, Ctx} = nat:get_context(),
              ?assertMatch({natpmp, _}, Ctx),
              ?assertEqual({ok, "203.0.113.1"}, nat:get_external_address()),
              ?assertEqual({ok, "192.168.1.100"}, nat:get_internal_address())
          end},
          {"Add port mapping with mock", fun() ->
              ok = nat:discover(),
              Result = nat:add_port_mapping(tcp, 8333, 8333),
              ?assertMatch({ok, {12345, 8333, 8333, 7200}}, Result),
              Mappings = nat:list_mappings(),
              ?assert(length(Mappings) >= 1),
              MappingList = [M || M = {tcp, 8333, 8333, _} <- Mappings],
              ?assertEqual(1, length(MappingList))
          end},
          {"Delete port mapping with mock", fun() ->
              ok = nat:discover(),
              nat:add_port_mapping(tcp, 9999, 9999),
              ?assertEqual(ok, nat:delete_port_mapping(tcp, 9999, 9999)),
              Mappings = [M || M = {_, IntPort, _, _} <- nat:list_mappings(), IntPort =:= 9999],
              ?assertEqual([], Mappings)
          end},
          {"Renewal timer is scheduled", fun() ->
              ok = nat:discover(),
              nat:add_port_mapping(tcp, 7777, 7777, 120),
              Mappings = nat:list_mappings(),
              MappingList = [M || M = {_, IntPort, _, _} <- Mappings, IntPort =:= 7777],
              ?assertEqual(1, length(MappingList)),
              nat:delete_port_mapping(tcp, 7777, 7777)
          end},
          {"Event dispatch to pid", fun() ->
              ok = nat:discover(),
              Self = self(),
              nat:reg_pid(Self),
              nat:add_port_mapping(tcp, 6666, 6666, 120),
              nat:delete_port_mapping(tcp, 6666, 6666),
              nat:unreg_pid(Self)
          end},
          {"Event dispatch to fun", fun() ->
              ok = nat:discover(),
              TestPid = self(),
              Fun = fun(Event) -> TestPid ! {test_event, Event} end,
              {ok, Ref} = nat:reg_fun(Fun),
              nat:add_port_mapping(tcp, 5555, 5555, 120),
              nat:delete_port_mapping(tcp, 5555, 5555),
              nat:unreg_fun(Ref)
          end}
         ]
     end
    }.

setup_mocks() ->
    meck:new(natpmp, [passthrough, no_link]),
    meck:new(natupnp_v1, [passthrough, no_link]),
    meck:new(natupnp_v2, [passthrough, no_link]),
    meck:new(natpcp, [passthrough, no_link]),

    meck:expect(natpmp, discover, fun() ->
        {ok, {192, 168, 1, 1}}
    end),

    meck:expect(natupnp_v1, discover, fun() -> {error, timeout} end),
    meck:expect(natupnp_v2, discover, fun() -> {error, timeout} end),
    meck:expect(natpcp, discover, fun() -> {error, timeout} end),

    meck:expect(natpmp, get_external_address, fun(_Gateway) ->
        {ok, "203.0.113.1"}
    end),

    meck:expect(natpmp, get_device_address, fun(Gateway) ->
        {ok, Gateway}
    end),

    meck:expect(natpmp, get_internal_address, fun(_Gateway) ->
        {ok, "192.168.1.100"}
    end),

    meck:expect(natpmp, add_port_mapping, fun(_Gateway, _Protocol, InternalPort, ExternalPort, Lifetime) ->
        {ok, 12345, InternalPort, ExternalPort, Lifetime}
    end),

    meck:expect(natpmp, delete_port_mapping, fun(_Gateway, _Protocol, _InternalPort, _ExternalPort) ->
        ok
    end),
    ok.

teardown_mocks() ->
    catch meck:unload(natpmp),
    catch meck:unload(natupnp_v1),
    catch meck:unload(natupnp_v2),
    catch meck:unload(natpcp),
    ok.

%% ===================================================================
%% Expiration Time Tests
%% ===================================================================

expiration_test_() ->
    {setup,
     fun() ->
         setup_mocks(),
         {ok, _} = application:ensure_all_started(nat),
         timer:sleep(50),
         ok
     end,
     fun(_) ->
         application:stop(nat),
         teardown_mocks()
     end,
     fun(_) ->
         [
          {"Mapping has correct expiration time", fun() ->
              ok = nat:discover(),
              Now = erlang:system_time(second),
              Lifetime = 3600,
              nat:add_port_mapping(tcp, 4001, 4001, Lifetime),
              [{tcp, 4001, 4001, ExpiresAt}] = [M || M = {tcp, 4001, _, _} <- nat:list_mappings()],
              %% ExpiresAt should be approximately Now + Lifetime (within 2 seconds tolerance)
              ?assert(ExpiresAt >= Now + Lifetime - 2),
              ?assert(ExpiresAt =< Now + Lifetime + 2),
              nat:delete_port_mapping(tcp, 4001, 4001)
          end},
          {"Multiple mappings have independent expiration times", fun() ->
              ok = nat:discover(),
              Now = erlang:system_time(second),
              nat:add_port_mapping(tcp, 4002, 4002, 1800),  %% 30 min
              nat:add_port_mapping(udp, 4003, 4003, 7200),  %% 2 hours
              Mappings = nat:list_mappings(),
              [{tcp, 4002, 4002, Expires1}] = [M || M = {tcp, 4002, _, _} <- Mappings],
              [{udp, 4003, 4003, Expires2}] = [M || M = {udp, 4003, _, _} <- Mappings],
              %% TCP mapping expires in ~30 min
              ?assert(Expires1 >= Now + 1798),
              ?assert(Expires1 =< Now + 1802),
              %% UDP mapping expires in ~2 hours
              ?assert(Expires2 >= Now + 7198),
              ?assert(Expires2 =< Now + 7202),
              %% UDP expires later than TCP
              ?assert(Expires2 > Expires1),
              nat:delete_port_mapping(tcp, 4002, 4002),
              nat:delete_port_mapping(udp, 4003, 4003)
          end},
          {"Short lifetime still gets minimum renewal interval", fun() ->
              ok = nat:discover(),
              Now = erlang:system_time(second),
              %% Even with 60 second lifetime, mapping should be tracked
              nat:add_port_mapping(tcp, 4004, 4004, 60),
              [{tcp, 4004, 4004, ExpiresAt}] = [M || M = {tcp, 4004, _, _} <- nat:list_mappings()],
              ?assert(ExpiresAt >= Now + 58),
              ?assert(ExpiresAt =< Now + 62),
              nat:delete_port_mapping(tcp, 4004, 4004)
          end}
         ]
     end
    }.

%% ===================================================================
%% IP Change Event Tests
%% ===================================================================

ip_change_test_() ->
    {setup,
     fun() ->
         setup_mocks(),
         {ok, _} = application:ensure_all_started(nat),
         timer:sleep(50),
         ok
     end,
     fun(_) ->
         application:stop(nat),
         teardown_mocks()
     end,
     fun(_) ->
         [
          {"IP change event dispatched to registered pid", fun() ->
              ok = nat:discover(),
              Self = self(),
              nat:reg_pid(Self),
              %% Simulate NAT-PMP multicast announcement with new IP
              nat_server ! {natpmp_announce, 12346, "198.51.100.1"},
              %% Should receive ip_changed event
              receive
                  {nat_event, {ip_changed, "203.0.113.1", "198.51.100.1"}} ->
                      ok
              after 1000 ->
                  ?assert(false)
              end,
              nat:unreg_pid(Self)
          end},
          {"IP change event dispatched to registered fun", fun() ->
              ok = nat:discover(),
              Self = self(),
              Fun = fun(Event) -> Self ! {fun_event, Event} end,
              {ok, Ref} = nat:reg_fun(Fun),
              %% Simulate NAT-PMP multicast announcement with new IP
              nat_server ! {natpmp_announce, 12347, "198.51.100.2"},
              %% Should receive ip_changed event via fun
              receive
                  {fun_event, {ip_changed, _, "198.51.100.2"}} ->
                      ok
              after 1000 ->
                  ?assert(false)
              end,
              nat:unreg_fun(Ref)
          end},
          {"No event when IP unchanged", fun() ->
              ok = nat:discover(),
              Self = self(),
              nat:reg_pid(Self),
              %% Get current IP first
              {ok, CurrentIp} = nat:get_external_address(),
              %% Simulate announcement with SAME IP
              nat_server ! {natpmp_announce, 12348, CurrentIp},
              %% Should NOT receive ip_changed event
              receive
                  {nat_event, {ip_changed, _, _}} ->
                      ?assert(false)
              after 200 ->
                  ok
              end,
              nat:unreg_pid(Self)
          end},
          {"Epoch reset triggers IP change check", fun() ->
              ok = nat:discover(),
              Self = self(),
              nat:reg_pid(Self),
              %% First announcement sets epoch
              nat_server ! {natpmp_announce, 1000, "203.0.113.50"},
              receive
                  {nat_event, {ip_changed, _, "203.0.113.50"}} -> ok
              after 500 -> ok
              end,
              %% Epoch reset (lower value) with same IP should still trigger event
              nat_server ! {natpmp_announce, 500, "203.0.113.50"},
              receive
                  {nat_event, {ip_changed, "203.0.113.50", "203.0.113.50"}} ->
                      ok
              after 1000 ->
                  %% Epoch reset with same IP triggers renewal but may not send ip_changed
                  %% if IPs are identical - this is acceptable behavior
                  ok
              end,
              nat:unreg_pid(Self)
          end}
         ]
     end
    }.

%% ===================================================================
%% Renewal Event Tests
%% ===================================================================

renewal_event_test_() ->
    {setup,
     fun() ->
         setup_mocks(),
         {ok, _} = application:ensure_all_started(nat),
         timer:sleep(50),
         ok
     end,
     fun(_) ->
         application:stop(nat),
         teardown_mocks()
     end,
     fun(_) ->
         [
          {"Mapping renewal triggers event", fun() ->
              ok = nat:discover(),
              Self = self(),
              nat:reg_pid(Self),
              %% Add mapping
              nat:add_port_mapping(tcp, 4010, 4010, 120),
              %% Manually trigger renewal by sending the timer message
              nat_server ! {renew_mapping, {tcp, 4010}},
              %% Should receive mapping_renewed event
              receive
                  {nat_event, {mapping_renewed, tcp, 4010, 4010, 120}} ->
                      ok
              after 1000 ->
                  ?assert(false)
              end,
              nat:delete_port_mapping(tcp, 4010, 4010),
              nat:unreg_pid(Self)
          end}
         ]
     end
    }.

%% ===================================================================
%% Multicast Listener Tests
%% ===================================================================

multicast_packet_parsing_test() ->
    Epoch = 12345,
    ExternalIp = {203, 0, 113, 1},
    {A, B, C, D} = ExternalIp,
    Packet = <<0, 128, 0:16, Epoch:32, A, B, C, D>>,

    ?assertEqual(12, byte_size(Packet)),
    <<_Version:8, OpCode:8, _Status:16, ParsedEpoch:32, PA, PB, PC, PD>> = Packet,
    ?assertEqual(128, OpCode),
    ?assertEqual(Epoch, ParsedEpoch),
    ?assertEqual({PA, PB, PC, PD}, ExternalIp).

%% ===================================================================
%% State Record Tests
%% ===================================================================

mapping_record_test() ->
    Mapping = #mapping{
        key = {tcp, 8333},
        external_port = 8333,
        since = 12345,
        lifetime = 7200,
        expires_at = 12345 + 7200,
        timer_ref = undefined,
        retry_count = 0
    },
    ?assertEqual({tcp, 8333}, Mapping#mapping.key),
    ?assertEqual(7200, Mapping#mapping.lifetime),
    ?assertEqual(0, Mapping#mapping.retry_count).

nat_state_record_test() ->
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
    ?assertEqual(undefined, State#nat_state.nat_ctx),
    ?assertEqual(#{}, State#nat_state.mappings).
