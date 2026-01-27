%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.

-module(natpmp_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Response Parsing Tests
%% ===================================================================

%% Test parsing external address response (opcode 0)
parse_external_address_response_test() ->
    %% Response format: version(1), opcode(128), status(2), since(4), ip(4)
    %% OpCode 0 (external address) response has opcode 128 (0 + 128)
    Response = <<0, 128, 0:16, 12345:32, 203, 0, 113, 1>>,
    setup_mock_for_response(Response),
    Result = natpmp:get_external_address_with_epoch({192,168,1,1}),
    teardown_mock(),
    ?assertMatch({ok, {203, 0, 113, 1}, 12345}, Result).

%% Test that get_external_address returns string (for backwards compatibility)
get_external_address_returns_string_test() ->
    Response = <<0, 128, 0:16, 12345:32, 203, 0, 113, 1>>,
    setup_mock_for_response(Response),
    Result = natpmp:get_external_address({192,168,1,1}),
    teardown_mock(),
    ?assertEqual({ok, "203.0.113.1"}, Result).

%% Test parsing mapping response (opcode 1 = UDP, 2 = TCP)
parse_mapping_response_test() ->
    %% Response format: version(1), opcode(129/130), status(2), since(4),
    %%                  internal_port(2), external_port(2), lifetime(4)
    %% TCP (opcode 2) response has opcode 130 (2 + 128)
    Response = <<0, 130, 0:16, 12345:32, 8333:16, 8333:16, 7200:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:add_port_mapping({192,168,1,1}, tcp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertMatch({ok, 12345, 8333, 8333, 7200}, Result).

%% Test parsing UDP mapping response
parse_udp_mapping_response_test() ->
    %% UDP (opcode 1) response has opcode 129 (1 + 128)
    Response = <<0, 129, 0:16, 12345:32, 8333:16, 8334:16, 7200:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:add_port_mapping({192,168,1,1}, udp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertMatch({ok, 12345, 8333, 8334, 7200}, Result).

%% ===================================================================
%% Error Response Tests
%% ===================================================================

%% Test unsupported version error (status 1)
unsupported_version_test() ->
    Response = <<0, 128, 1:16, 0:32, 0, 0, 0, 0>>,
    setup_mock_for_response(Response),
    Result = natpmp:get_external_address_with_epoch({192,168,1,1}),
    teardown_mock(),
    ?assertEqual({error, unsupported_version}, Result).

%% Test not authorized error (status 2)
not_authorized_test() ->
    Response = <<0, 130, 2:16, 0:32, 0:16, 0:16, 0:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:add_port_mapping({192,168,1,1}, tcp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertEqual({error, not_authorized}, Result).

%% Test network failure error (status 3)
network_failure_test() ->
    Response = <<0, 128, 3:16, 0:32, 0, 0, 0, 0>>,
    setup_mock_for_response(Response),
    Result = natpmp:get_external_address_with_epoch({192,168,1,1}),
    teardown_mock(),
    ?assertEqual({error, network_failure}, Result).

%% Test out of resources error (status 4)
out_of_resources_test() ->
    Response = <<0, 130, 4:16, 0:32, 0:16, 0:16, 0:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:add_port_mapping({192,168,1,1}, tcp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertEqual({error, out_of_resource}, Result).

%% Test bad response (wrong opcode)
bad_response_opcode_test() ->
    %% Response with wrong opcode (129 instead of 130 for TCP)
    Response = <<0, 129, 0:16, 12345:32, 8333:16, 8333:16, 7200:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:add_port_mapping({192,168,1,1}, tcp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertEqual({error, bad_response}, Result).

%% ===================================================================
%% Delete Mapping Tests
%% ===================================================================

%% Test delete port mapping (lifetime = 0)
delete_port_mapping_test() ->
    %% Successful delete returns internal_port, 0, 0
    Response = <<0, 130, 0:16, 12345:32, 8333:16, 0:16, 0:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:delete_port_mapping({192,168,1,1}, tcp, 8333, 8333),
    teardown_mock(),
    ?assertEqual(ok, Result).

%% Test delete all mappings (internal_port = 0, external_port = 0, lifetime = 0)
delete_all_mappings_test() ->
    Response = <<0, 130, 0:16, 12345:32, 0:16, 0:16, 0:32>>,
    setup_mock_for_mapping_response(Response),
    Result = natpmp:delete_all_mappings({192,168,1,1}, tcp),
    teardown_mock(),
    ?assertEqual(ok, Result).

%% ===================================================================
%% Helper Functions
%% ===================================================================

setup_mock_for_response(Response) ->
    meck:new(gen_udp, [unstick, passthrough, no_link]),
    meck:expect(gen_udp, send,
        fun(Socket, Ip, Port, _Msg) ->
            self() ! {udp, Socket, Ip, Port, Response},
            ok
        end).

setup_mock_for_mapping_response(Response) ->
    setup_mock_for_response(Response).

teardown_mock() ->
    catch meck:unload(gen_udp).
