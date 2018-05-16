%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016 Benoît Chesneau <benoitc@refuge.io>

-module(natupnp_v1).

-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).
-export([status_info/1]).

-include("nat.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-record(nat_upnp, {
          service_url,
          ip}).


-type nat_upnp() :: #nat_upnp{}.
-export_type([nat_upnp/0]).


%% @doc discover the gateway and our IP to associate
-spec discover() -> {ok, Context:: nat_upnp()}
                    | {error, term()}.
discover() ->
    _ = application:start(inets),
    _ = rand_compat:seed(erlang:phash2([node()]),
                         erlang:monotonic_time(),
                         erlang:unique_integer()),
    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),

    ST = <<"urn:schemas-upnp-org:device:InternetGatewayDevice:1" >>,

    MSearch = [<<"M-SEARCH * HTTP/1.1\r\n"
                 "HOST: 239.255.255.250:1900\r\n"
                 "MAN: \"ssdp:discover\"\r\n"
                 "ST: ">>,  ST, <<"\r\n"
                                  "MX: 3"
                                  "\r\n\r\n">>],

    try
        discover1(Sock, iolist_to_binary(MSearch), 3)
    after
        gen_udp:close(Sock)
    end.


discover1(_Sock, _MSearch, ?NAT_TRIES) ->
    timeout;
discover1(Sock, MSearch, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = ?NAT_INITIAL_MS bsl Tries,
    ok = gen_udp:send(Sock, "239.255.255.250", 1900, MSearch),
    receive
        {udp, _Sock, Ip, _Port, Packet} ->
            case get_location(Packet) of
                error ->
                    discover1(Sock, MSearch, Tries-1);
                Location ->
                    case get_service_url(binary_to_list(Location)) of
                        {ok, Url} ->
                            MyIp = inet_ext:get_internal_address(Ip),
                            {ok, #nat_upnp{service_url=Url, ip=MyIp}};
                        Error ->
                            Error
                    end
            end
    after Timeout ->
              discover1(Sock, MSearch, Tries+1)
    end.


get_device_address(#nat_upnp{service_url=Url}) ->
    Res = case http_uri:parse(Url) of
        {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
            inet:getaddr(Host, inet);
        {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query, _Fragment}} ->
            inet:getaddr(Host, inet);
        Error -> Error
    end,
    %% unparse the IP
    case Res of
        {ok, Ip} -> {ok, inet:ntoa(Ip)};
        _ -> Res
    end.


get_external_address(#nat_upnp{service_url=Url}) ->
    Message = "<u:GetExternalIPAddress xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "</u:GetExternalIPAddress>",
    case nat_lib:soap_request(Url, "GetExternalIPAddress", Message) of
        {ok, Body} ->
            {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),

            [Infos | _] = xmerl_xpath:string("//s:Envelope/s:Body/"
                                             "u:GetExternalIPAddressResponse", Xml),

            IP = extract_txt(
                   xmerl_xpath:string("NewExternalIPAddress/text()",
                                      Infos)
                  ),

            {ok, IP};
        Error ->
            Error
    end.

get_internal_address(#nat_upnp{ip=Ip}) ->
    {ok, Ip}.


%% @doc Add a port mapping with default lifetime to 3600 seconds
-spec add_port_mapping(Context :: nat_upnp(),
                       Protocol:: tcp | udp, ExternalPort :: integer(),
                       InternalPort :: integer()) -> ok | {error, term()}.
add_port_mapping(Context, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Context, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

%% @doc Add a port mapping and release after Timeout
-spec add_port_mapping(Context :: nat_upnp_proto:nat_upnp(),
                       Protocol:: tcp | udp, InternalPort :: integer(),
                       ExternalPort :: integer(),
                       Lifetime :: integer()) -> ok | {error, term()}.
add_port_mapping(Ctx, Protocol, InternalPort, ExternalPort, 0) ->
    delete_port_mapping(Ctx, Protocol, InternalPort, ExternalPort);
add_port_mapping(Ctx, Protocol0, InternalPort, ExternalPort, Lifetime) ->
    Protocol = protocol(Protocol0),
    case ExternalPort of
        0 ->
            random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, nil, 3);
        _ ->
            add_port_mapping1(Ctx,Protocol, InternalPort, ExternalPort, Lifetime)
    end.

random_port_mapping(_Ctx, _Protocol, _InternalPort, _Lifetime, Error, 0) ->
    Error;
random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, _LastError, Tries) ->
    ExternalPort = nat_lib:random_port(),
    Res = add_port_mapping1(Ctx, Protocol, InternalPort, ExternalPort, Lifetime),
    case Res of
        {ok, _, _, _, _} ->
            Res;
        Error ->
            random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, Error,
                                Tries -1)
    end.

add_port_mapping1(#nat_upnp{ip=Ip, service_url=Url}, Protocol, InternalPort,
                 ExternalPort, Lifetime) ->
    Description = Ip ++ "_" ++ Protocol ++ "_" ++ integer_to_list(InternalPort),
    Msg = "<u:AddPortMapping xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++  integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "<NewInternalPort>" ++ integer_to_list(InternalPort) ++
    "</NewInternalPort>"
    "<NewInternalClient>" ++ Ip ++ "</NewInternalClient>"
    "<NewEnabled>1</NewEnabled>"
    "<NewPortMappingDescription>" ++ Description ++
    "</NewPortMappingDescription>"
    "<NewLeaseDuration>" ++ integer_to_list(Lifetime) ++
    "</NewLeaseDuration></u:AddPortMapping>",

    Start = nat_lib:timestamp(),
    case nat_lib:soap_request(Url, "AddPortMapping", Msg) of
        {ok, _} ->
            Now = nat_lib:timestamp(),
            MappingLifetime = Lifetime - (Now - Start),
            {ok, Now, InternalPort, ExternalPort, MappingLifetime};
        Error -> Error
    end.

%% @doc Delete a port mapping from the router
-spec delete_port_mapping(Context :: nat_upnp(),
                          Protocol :: tcp | udp, InternalPort :: integer(),
                          ExternalPort :: integer())
-> ok | {error, term()}.
delete_port_mapping(#nat_upnp{service_url=Url}, Protocol0, _InternalPort, ExternalPort) ->
    Protocol = protocol(Protocol0),
    Msg = "<u:DeletePortMapping xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++ integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "</u:DeletePortMapping>",

    case nat_lib:soap_request(Url, "DeletePortMapping", Msg) of
        {ok, _} -> ok;
        Error -> Error
    end.


%% @doc get router status
-spec status_info(Context :: nat_upnp())
-> {Status::string(), LastConnectionError::string(), Uptime::string()}
   | {error, term()}.
status_info(#nat_upnp{service_url=Url}) ->
    Message = "<u:GetStatusInfo xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "</u:GetStatusInfo>",
    case nat_lib:soap_request(Url, "GetStatusInfo", Message) of
        {ok, Body} ->
            {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),

            [Infos | _] = xmerl_xpath:string("//s:Envelope/s:Body/"
                                             "u:GetStatusInfoResponse", Xml),

            Status = extract_txt(
                       xmerl_xpath:string("NewConnectionStatus/text()",
                                          Infos)
                      ),

            LastConnectionError = extract_txt(
                                    xmerl_xpath:string("NewLastConnectionError/text()",
                                                       Infos)
                                   ),

            Uptime = extract_txt(
                       xmerl_xpath:string("NewUptime/text()",
                                          Infos)
                      ),
            {Status, LastConnectionError, Uptime};
        Error ->
            Error
    end.


%% internals

get_location(Raw) ->
    case erlang:decode_packet(httph_bin, Raw, []) of
        {ok, {http_error, _}, Rest} ->
            get_location(Rest);
        {ok, {http_header, _, 'Location', _, Location}, _Rest} ->
            Location;
        {ok, {http_header, _, _H, _, _V}, Rest} ->
            get_location(Rest);
        _ ->
            error
    end.

get_service_url(RootUrl) ->
    case httpc:request(RootUrl) of
        {ok, {{_, 200, _}, _, Body}} ->
            {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
            [Device | _] = xmerl_xpath:string("//device", Xml),
            case device_type(Device) of
                "urn:schemas-upnp-org:device:InternetGatewayDevice:1" ->
                    get_wan_device(Device, RootUrl);
                _ ->
                    {error,  no_gateway_device}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, integer_to_list(StatusCode)};
        Error ->
            Error
    end.

get_wan_device(D, RootUrl) ->
    case get_device(D, "urn:schemas-upnp-org:device:WANDevice:1") of
        {ok, D1} ->
            get_connection_device(D1, RootUrl);
        _ ->
            {erro, no_wan_device}
    end.

get_connection_device(D, RootUrl) ->
    case get_device(D, "urn:schemas-upnp-org:device:WANConnectionDevice:1") of
        {ok, D1} ->
            get_connection_url(D1, RootUrl);

        _ ->
            {error, no_wanconnection_device}
    end.


get_connection_url(D, RootUrl) ->
    case get_service(D, "urn:schemas-upnp-org:service:WANIPConnection:1") of
        {ok, S} ->
            Url = extract_txt(xmerl_xpath:string("controlURL/text()",
                                                 S)),
            case split(RootUrl, "://") of
                [Scheme, Rest] ->
                    case split(Rest, "/") of
                        [NetLoc| _] ->
                            CtlUrl = Scheme ++ "://" ++ NetLoc ++ Url,
                            {ok, CtlUrl};
                        _Else ->
                            {error, invalid_control_url}
                    end;
                _Else ->

                    {error, invalid_control_url}

            end;
        _ ->
            {error, no_wanipconnection}
    end.


get_device(Device, DeviceType) ->
    DeviceList = xmerl_xpath:string("deviceList/device", Device),
    find_device(DeviceList, DeviceType).

find_device([], _DeviceType) ->
    false;
find_device([D | Rest], DeviceType) ->
    case device_type(D) of
        DeviceType ->
            {ok, D};
        _ ->
            find_device(Rest, DeviceType)
    end.

get_service(Device, ServiceType) ->
    ServiceList = xmerl_xpath:string("serviceList/service", Device),
    find_service(ServiceList, ServiceType).

find_service([], _ServiceType) ->
    false;
find_service([S | Rest], ServiceType) ->
    case extract_txt(xmerl_xpath:string("serviceType/text()", S)) of
        ServiceType ->
            {ok, S};
        _ ->
            find_service(Rest, ServiceType)
    end.

device_type(Device) ->
    extract_txt(xmerl_xpath:string("deviceType/text()", Device)).

%% Given a xml text node, extract its text value.
extract_txt(Xml) ->
    [T|_] = [X#xmlText.value || X <- Xml, is_record(X, xmlText)],
    T.

split(String, Pattern) ->
    re:split(String, Pattern, [{return, list}]).


protocol(Protocol) ->
    case lists:member(Protocol, [tcp, udp]) of
        true -> ok;
        false -> error(bad_protocol)
    end,
    string:to_upper(atom_to_list(Protocol)).
