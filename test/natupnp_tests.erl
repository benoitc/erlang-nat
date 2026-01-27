%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.

-module(natupnp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("nat.hrl").

%% ===================================================================
%% Port Mapping Entry Parsing Tests
%% ===================================================================

%% Test parsing a complete port mapping entry response
parse_port_mapping_entry_test() ->
    XmlBody = "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" "
        "s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
        "<s:Body>"
        "<u:GetGenericPortMappingEntryResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewRemoteHost></NewRemoteHost>"
        "<NewExternalPort>8333</NewExternalPort>"
        "<NewProtocol>TCP</NewProtocol>"
        "<NewInternalPort>8333</NewInternalPort>"
        "<NewInternalClient>192.168.1.100</NewInternalClient>"
        "<NewEnabled>1</NewEnabled>"
        "<NewPortMappingDescription>erlang-nat test</NewPortMappingDescription>"
        "<NewLeaseDuration>7200</NewLeaseDuration>"
        "</u:GetGenericPortMappingEntryResponse>"
        "</s:Body>"
        "</s:Envelope>",

    {ok, Mapping} = natupnp_v1:parse_port_mapping_entry(XmlBody),

    ?assertEqual("", Mapping#port_mapping.remote_host),
    ?assertEqual(8333, Mapping#port_mapping.external_port),
    ?assertEqual(tcp, Mapping#port_mapping.protocol),
    ?assertEqual(8333, Mapping#port_mapping.internal_port),
    ?assertEqual("192.168.1.100", Mapping#port_mapping.internal_client),
    ?assertEqual(true, Mapping#port_mapping.enabled),
    ?assertEqual("erlang-nat test", Mapping#port_mapping.description),
    ?assertEqual(7200, Mapping#port_mapping.lease_duration).

%% Test parsing UDP protocol
parse_port_mapping_udp_test() ->
    XmlBody = "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetGenericPortMappingEntryResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewRemoteHost></NewRemoteHost>"
        "<NewExternalPort>53</NewExternalPort>"
        "<NewProtocol>UDP</NewProtocol>"
        "<NewInternalPort>53</NewInternalPort>"
        "<NewInternalClient>192.168.1.1</NewInternalClient>"
        "<NewEnabled>1</NewEnabled>"
        "<NewPortMappingDescription>DNS</NewPortMappingDescription>"
        "<NewLeaseDuration>0</NewLeaseDuration>"
        "</u:GetGenericPortMappingEntryResponse>"
        "</s:Body>"
        "</s:Envelope>",

    {ok, Mapping} = natupnp_v1:parse_port_mapping_entry(XmlBody),
    ?assertEqual(udp, Mapping#port_mapping.protocol).

%% Test parsing disabled mapping
parse_port_mapping_disabled_test() ->
    XmlBody = "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetGenericPortMappingEntryResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewRemoteHost></NewRemoteHost>"
        "<NewExternalPort>8080</NewExternalPort>"
        "<NewProtocol>TCP</NewProtocol>"
        "<NewInternalPort>8080</NewInternalPort>"
        "<NewInternalClient>192.168.1.50</NewInternalClient>"
        "<NewEnabled>0</NewEnabled>"
        "<NewPortMappingDescription>disabled</NewPortMappingDescription>"
        "<NewLeaseDuration>3600</NewLeaseDuration>"
        "</u:GetGenericPortMappingEntryResponse>"
        "</s:Body>"
        "</s:Envelope>",

    {ok, Mapping} = natupnp_v1:parse_port_mapping_entry(XmlBody),
    ?assertEqual(false, Mapping#port_mapping.enabled).

%% Test parsing with remote host specified
parse_port_mapping_with_remote_host_test() ->
    XmlBody = "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetGenericPortMappingEntryResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewRemoteHost>10.0.0.5</NewRemoteHost>"
        "<NewExternalPort>22</NewExternalPort>"
        "<NewProtocol>TCP</NewProtocol>"
        "<NewInternalPort>22</NewInternalPort>"
        "<NewInternalClient>192.168.1.10</NewInternalClient>"
        "<NewEnabled>1</NewEnabled>"
        "<NewPortMappingDescription>SSH</NewPortMappingDescription>"
        "<NewLeaseDuration>0</NewLeaseDuration>"
        "</u:GetGenericPortMappingEntryResponse>"
        "</s:Body>"
        "</s:Envelope>",

    {ok, Mapping} = natupnp_v1:parse_port_mapping_entry(XmlBody),
    ?assertEqual("10.0.0.5", Mapping#port_mapping.remote_host).

%% Test parsing invalid response
parse_invalid_response_test() ->
    XmlBody = "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<SomeOtherResponse></SomeOtherResponse>"
        "</s:Body>"
        "</s:Envelope>",

    Result = natupnp_v1:parse_port_mapping_entry(XmlBody),
    ?assertEqual({error, invalid_response}, Result).

%% ===================================================================
%% External Address Parsing Tests
%% ===================================================================

%% Test parsing external address response
external_address_response_test() ->
    setup_hackney_mock(<<
        "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetExternalIPAddressResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewExternalIPAddress>203.0.113.1</NewExternalIPAddress>"
        "</u:GetExternalIPAddressResponse>"
        "</s:Body>"
        "</s:Envelope>"
    >>),

    Ctx = #nat_upnp{service_url = "http://192.168.1.1:5000/ctl/IPConn", ip = "192.168.1.100"},
    Result = natupnp_v1:get_external_address(Ctx),
    teardown_mock(),

    ?assertEqual({ok, "203.0.113.1"}, Result).

%% ===================================================================
%% Port Mapping Count Parsing Tests
%% ===================================================================

port_mapping_count_response_test() ->
    setup_hackney_mock(<<
        "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetPortMappingNumberOfEntriesResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewPortMappingNumberOfEntries>5</NewPortMappingNumberOfEntries>"
        "</u:GetPortMappingNumberOfEntriesResponse>"
        "</s:Body>"
        "</s:Envelope>"
    >>),

    Ctx = #nat_upnp{service_url = "http://192.168.1.1:5000/ctl/IPConn", ip = "192.168.1.100"},
    Result = natupnp_v1:get_port_mapping_count(Ctx),
    teardown_mock(),

    ?assertEqual({ok, 5}, Result).

%% ===================================================================
%% Status Info Parsing Tests
%% ===================================================================

status_info_response_test() ->
    setup_hackney_mock(<<
        "<?xml version=\"1.0\"?>"
        "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        "<s:Body>"
        "<u:GetStatusInfoResponse xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewConnectionStatus>Connected</NewConnectionStatus>"
        "<NewLastConnectionError>ERROR_NONE</NewLastConnectionError>"
        "<NewUptime>123456</NewUptime>"
        "</u:GetStatusInfoResponse>"
        "</s:Body>"
        "</s:Envelope>"
    >>),

    Ctx = #nat_upnp{service_url = "http://192.168.1.1:5000/ctl/IPConn", ip = "192.168.1.100"},
    Result = natupnp_v1:status_info(Ctx),
    teardown_mock(),

    ?assertEqual({"Connected", "ERROR_NONE", "123456"}, Result).

%% ===================================================================
%% Helper Functions
%% ===================================================================

setup_hackney_mock(ResponseBody) ->
    meck:new(hackney, [passthrough, no_link]),
    meck:expect(hackney, request,
        fun(post, _Url, _Headers, _Body, _Opts) ->
            {ok, 200, [], ResponseBody}
        end).

teardown_mock() ->
    catch meck:unload(hackney).
