%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.

-module(natpcp_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% IP Address Conversion Tests
%% ===================================================================

%% Test IPv4 to PCP binary conversion (IPv4-mapped IPv6)
ipv4_to_pcp_binary_test() ->
    %% IPv4 192.168.1.1 should become ::ffff:192.168.1.1
    Expected = <<0:80, 16#ffff:16, 192, 168, 1, 1>>,
    ?assertEqual(Expected, natpcp:ip_to_pcp_binary({192,168,1,1})).

%% Test IPv6 to PCP binary conversion
ipv6_to_pcp_binary_test() ->
    %% IPv6 2001:db8::1
    Expected = <<16#2001:16, 16#0db8:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>,
    ?assertEqual(Expected, natpcp:ip_to_pcp_binary({16#2001, 16#0db8, 0, 0, 0, 0, 0, 1})).

%% Test PCP binary to IPv4 conversion
pcp_binary_to_ipv4_test() ->
    %% ::ffff:203.0.113.1 should become {203, 0, 113, 1}
    Binary = <<0:80, 16#ffff:16, 203, 0, 113, 1>>,
    ?assertEqual({203, 0, 113, 1}, natpcp:pcp_binary_to_ip(Binary)).

%% Test PCP binary to IPv6 conversion
pcp_binary_to_ipv6_test() ->
    %% 2001:db8::1
    Binary = <<16#2001:16, 16#0db8:16, 0:16, 0:16, 0:16, 0:16, 0:16, 1:16>>,
    ?assertEqual({16#2001, 16#0db8, 0, 0, 0, 0, 0, 1}, natpcp:pcp_binary_to_ip(Binary)).

%% ===================================================================
%% ANNOUNCE Response Parsing Tests
%% ===================================================================

%% Test parsing successful ANNOUNCE response
parse_announce_success_test() ->
    %% ANNOUNCE response: version(1), opcode(128), reserved(1), result(1),
    %%                    lifetime(4), epoch(4), reserved(12)
    Response = <<2, 128, 0, 0, 0:32, 12345:32, 0:96>>,
    Result = natpcp:parse_response(Response, 0),
    ?assertEqual({ok, 12345}, Result).

%% Test parsing ANNOUNCE with error
parse_announce_unsupp_version_test() ->
    Response = <<2, 128, 0, 1, 0:32, 0:32, 0:96>>,  %% Result code 1 = UNSUPP_VERSION
    Result = natpcp:parse_response(Response, 0),
    ?assertEqual({error, unsupp_version}, Result).

%% ===================================================================
%% MAP Response Parsing Tests
%% ===================================================================

%% Test parsing successful MAP response
parse_map_success_test() ->
    Nonce = <<1,2,3,4,5,6,7,8,9,10,11,12>>,
    ExternalAddr = <<0:80, 16#ffff:16, 203, 0, 113, 1>>,  %% 203.0.113.1
    %% MAP response format:
    %% version(1), opcode(129), reserved(1), result(1), lifetime(4), epoch(4), reserved(12),
    %% nonce(12), protocol(1), reserved(3), internal_port(2), external_port(2), external_addr(16)
    Response = <<2, 129, 0, 0,
                 7200:32,           %% lifetime
                 12345:32,          %% epoch
                 0:96,              %% reserved
                 Nonce/binary,      %% nonce
                 6,                 %% protocol (TCP)
                 0:24,              %% reserved
                 8333:16,           %% internal port
                 8333:16,           %% external port
                 ExternalAddr/binary>>,
    Result = natpcp:parse_response(Response, 1),
    ?assertMatch({ok, {203, 0, 113, 1}, 12345, 8333, 7200}, Result).

%% Test parsing MAP response with different external port
parse_map_different_port_test() ->
    Nonce = <<1,2,3,4,5,6,7,8,9,10,11,12>>,
    ExternalAddr = <<0:80, 16#ffff:16, 203, 0, 113, 1>>,
    Response = <<2, 129, 0, 0,
                 7200:32,
                 12345:32,
                 0:96,
                 Nonce/binary,
                 6,
                 0:24,
                 8333:16,
                 9999:16,           %% Different external port
                 ExternalAddr/binary>>,
    Result = natpcp:parse_response(Response, 1),
    ?assertMatch({ok, _, _, 9999, _}, Result).

%% ===================================================================
%% MAP Error Response Tests
%% ===================================================================

%% Test NOT_AUTHORIZED error
parse_map_not_authorized_test() ->
    Response = <<2, 129, 0, 2, 0:32, 0:32, 0:96, 0:288>>,  %% 288 bits = 36 bytes MAP payload
    Result = natpcp:parse_response(Response, 1),
    ?assertEqual({error, not_authorized}, Result).

%% Test NO_RESOURCES error
parse_map_no_resources_test() ->
    Response = <<2, 129, 0, 8, 0:32, 0:32, 0:96, 0:288>>,  %% Result code 8 = NO_RESOURCES
    Result = natpcp:parse_response(Response, 1),
    ?assertEqual({error, no_resources}, Result).

%% Test UNSUPP_PROTOCOL error
parse_map_unsupp_protocol_test() ->
    Response = <<2, 129, 0, 9, 0:32, 0:32, 0:96, 0:288>>,  %% Result code 9 = UNSUPP_PROTOCOL
    Result = natpcp:parse_response(Response, 1),
    ?assertEqual({error, unsupp_protocol}, Result).

%% Test CANNOT_PROVIDE_EXTERNAL error
parse_map_cannot_provide_external_test() ->
    Response = <<2, 129, 0, 11, 0:32, 0:32, 0:96, 0:288>>,  %% Result code 11
    Result = natpcp:parse_response(Response, 1),
    ?assertEqual({error, cannot_provide_external}, Result).

%% Test bad response (invalid format)
parse_bad_response_test() ->
    Response = <<1, 2, 3>>,  %% Too short
    Result = natpcp:parse_response(Response, 1),
    ?assertEqual({error, bad_response}, Result).

%% ===================================================================
%% Integration Tests with Mocked UDP
%% ===================================================================

%% Test add_port_mapping with mocked response
add_port_mapping_mocked_test() ->
    Nonce = crypto:strong_rand_bytes(12),
    ExternalAddr = <<0:80, 16#ffff:16, 203, 0, 113, 1>>,
    Response = <<2, 129, 0, 0,
                 7200:32,
                 12345:32,
                 0:96,
                 Nonce/binary,
                 6,
                 0:24,
                 8333:16,
                 8333:16,
                 ExternalAddr/binary>>,

    setup_pcp_mock(Response),
    Result = natpcp:add_port_mapping({192,168,1,1}, tcp, 8333, 8333, 7200),
    teardown_mock(),
    ?assertMatch({ok, 12345, 8333, 8333, 7200}, Result).

%% Test delete_port_mapping (lifetime = 0)
delete_port_mapping_mocked_test() ->
    Nonce = crypto:strong_rand_bytes(12),
    ExternalAddr = <<0:80, 16#ffff:16, 0, 0, 0, 0>>,
    Response = <<2, 129, 0, 0,
                 0:32,              %% lifetime = 0
                 12345:32,
                 0:96,
                 Nonce/binary,
                 6,
                 0:24,
                 8333:16,
                 0:16,              %% external port = 0
                 ExternalAddr/binary>>,

    setup_pcp_mock(Response),
    Result = natpcp:delete_port_mapping({192,168,1,1}, tcp, 8333, 8333),
    teardown_mock(),
    ?assertEqual(ok, Result).

%% ===================================================================
%% Helper Functions
%% ===================================================================

setup_pcp_mock(Response) ->
    meck:new(gen_udp, [unstick, passthrough, no_link]),
    meck:new(inet_ext, [passthrough, no_link]),
    meck:expect(gen_udp, send,
        fun(Socket, Ip, Port, _Msg) ->
            self() ! {udp, Socket, Ip, Port, Response},
            ok
        end),
    meck:expect(inet_ext, get_internal_address,
        fun(_Gateway) ->
            "192.168.1.100"
        end),
    meck:expect(inet_ext, parse_address,
        fun(Addr) when is_tuple(Addr) -> Addr;
           (Addr) when is_list(Addr) ->
            {ok, Ip} = inet:parse_address(Addr),
            Ip
        end).

teardown_mock() ->
    catch meck:unload(gen_udp),
    catch meck:unload(inet_ext).
