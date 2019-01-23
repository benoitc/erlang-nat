%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

%% @doc Client for UPnP Device Control Protocol Internet Gateway Device v2.
%%
%% documented in detail at: http://upnp.org/specs/gw/UPnP-gw-InternetGatewayDevice-v2-Device.pdf

-module(natupnp_v2).

-export([discover/1]).
-export([get_device_address/1]).
-export([get_external_address/2]).
-export([get_internal_address/1]).
-export([add_port_mapping/6]).
-export([delete_port_mapping/5]).
-export([get_port_mapping/4]).
-export([status_info/2]).


-include("nat.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% @doc discover the gateway and our IP to associate
-spec discover(HttpcProfile :: pid()) -> {ok, Context:: nat:nat_upnp()}
                    | {error, term()}.
discover(HttpcProfile) ->
    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),

    ST = <<"urn:schemas-upnp-org:device:InternetGatewayDevice:2" >>,

    MSearch = [<<"M-SEARCH * HTTP/1.1\r\n"
                 "HOST: 239.255.255.250:1900\r\n"
                 "MAN: \"ssdp:discover\"\r\n"
                 "ST: ">>,  ST, <<"\r\n"
                                  "MX: 3"
                                  "\r\n\r\n">>],

    try
        discover1(Sock, iolist_to_binary(MSearch), HttpcProfile, 3)
    after
        gen_udp:close(Sock)
    end.

discover1(_Sock, _MSearch, _HttpcProfile, ?NAT_TRIES) ->
    {error, timeout};
discover1(Sock, MSearch, HttpcProfile, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = ?NAT_INITIAL_MS bsl Tries,
    ok = gen_udp:send(Sock, "239.255.255.250", 1900, MSearch),
    receive
        {udp, _Sock, Ip, _Port, Packet} ->
            case get_location(Packet) of
                error ->
                    discover1(Sock, MSearch, HttpcProfile, Tries-1);
                Location ->
                    case get_service_url(binary_to_list(Location), HttpcProfile) of
                        {ok, Url} ->
                            MyIp = inet_ext:get_internal_address(Ip),
                            case get_natrsipstatus(Url, HttpcProfile) of
                              enabled ->
                                {ok, #nat_upnp{service_url=Url, ip=MyIp}};
                              disabled ->
                                {error, no_nat};
                              Other ->
                                Other
                            end;
                        Error ->
                            Error
                    end
            end
    after Timeout ->
              discover1(Sock, MSearch, HttpcProfile, Tries+1)
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


get_external_address(#nat_upnp{service_url=Url}, HttpcProfile) ->
    Message = "<u:GetExternalIPAddress xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "</u:GetExternalIPAddress>",
    case nat_lib:soap_request(Url, "GetExternalIPAddress", Message, HttpcProfile) of
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

%% @doc Add a port mapping and release after Timeout
-spec add_port_mapping(nat:nat_upnp(), nat:nat_protocol(),integer(), integer(), integer(), pid()) ->
    {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} | {error, any()}.
add_port_mapping(Ctx, Protocol0, InternalPort, ExternalPort, Lifetime, HttpcProfile) ->
    Protocol = protocol(Protocol0),
    case ExternalPort of
        0 ->
            random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, nil, HttpcProfile, 3);
        _ ->
            add_port_mapping1(Ctx,Protocol, InternalPort, ExternalPort, Lifetime, HttpcProfile)
    end.

random_port_mapping(_Ctx, _Protocol, _InternalPort, _Lifetime, Error, _HttpcProfile, 0) ->
    Error;
random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, _LastError, HttpcProfile, Tries) ->
    ExternalPort = nat_lib:random_port(),
    Res = add_port_mapping1(Ctx, Protocol, InternalPort, ExternalPort, Lifetime, HttpcProfile),
    case Res of
        {ok, _, _, _, _} ->
            Res;
        Error ->
            random_port_mapping(Ctx, Protocol, InternalPort, Lifetime, Error, HttpcProfile,
                                Tries -1)
    end.

add_port_mapping1(#nat_upnp{ip=Ip, service_url=Url} = NatCtx,
                  Protocol, InternalPort, ExternalPort,
                  Lifetime, HttpcProfile) when is_integer(Lifetime), Lifetime >= 0 ->
    Description = Ip ++ "_" ++ Protocol ++ "_" ++ integer_to_list(InternalPort),
    Msg = "<u:AddAnyPortMapping xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:2\">"
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
    {ok, IAddr} = inet:parse_address(Ip),
    Start = nat_lib:timestamp(),
    ok = httpc:set_option(socket_opts, [{ip, IAddr}], HttpcProfile),
    Result =
        case nat_lib:soap_request(Url, "AddAnyPortMapping", Msg, [], HttpcProfile) of
            {ok, Body} ->
                {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),

                [Resp | _] = xmerl_xpath:string("//s:Envelope/s:Body/"
                                                 "u:AddAnyPortMappingResponse", Xml),

                ReservedPort = extract_txt(
                           xmerl_xpath:string("NewReservedPort/text()",
                                              Resp)
                          ),

                Now = nat_lib:timestamp(),
                MappingLifetime = Lifetime - (Now - Start),
                {ok, Now, InternalPort, list_to_integer(ReservedPort), MappingLifetime};
             Error when Lifetime > 0 ->
                %% Try to repair error code 725 - OnlyPermanentLeasesSupported
                case only_permanent_lease_supported(Error) of
                    true ->
                        error_logger:info_msg("UPNP: only permanent lease supported~n", []),
                        add_port_mapping1(NatCtx, Protocol, InternalPort, ExternalPort, 0, HttpcProfile);
                    false ->
                        Error
                  end;
            Error ->
                Error
        end,
    ok = httpc:set_option(socket_opts, [], HttpcProfile),
    Result.

only_permanent_lease_supported({error, {http_error, "500", Body}}) ->
  {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
  [Error | _]  = xmerl_xpath:string("//s:Envelope/s:Body/s:Fault/detail/"
                                   "UPnPError", Xml),
  ErrorCode = extract_txt(
                xmerl_xpath:string("errorCode/text()", Error)
               ),

  case ErrorCode of
    "725" -> true;
    _ -> false
  end;
only_permanent_lease_supported(_) ->
  false.

%% @doc Delete a port mapping from the router
-spec delete_port_mapping(Context :: nat:nat_upnp(),
                          Protocol :: nat:nat_protocol(), InternalPort :: integer(),
                          ExternalPort :: integer(), HttpcProfile :: pid())
-> ok | {error, term()}.
delete_port_mapping(#nat_upnp{ip=Ip, service_url=Url}, Protocol0, _InternalPort, ExternalPort, HttpcProfile) ->
    Protocol = protocol(Protocol0),
    Msg = "<u:DeletePortMapping xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++ integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "</u:DeletePortMapping>",
    {ok, IAddr} = inet:parse_address(Ip),
    ok = httpc:set_option(socket_opts, [{ip, IAddr}], HttpcProfile),
    Result =
        case nat_lib:soap_request(Url, "DeletePortMapping", Msg, [], HttpcProfile) of
            {ok, _} -> ok;
            Error -> Error
        end,
    ok = httpc:set_option(socket_opts, [], HttpcProfile),
    Result.


%% @doc get specific port mapping for a well known port and protocol
-spec get_port_mapping(Context :: nat:nat_upnp(),
                       Protocol :: nat:nat_protocol(),
                       ExternalPort :: integer(),
                       HttpcProfile :: pid())
-> {ok, InternalPort :: integer(), InternalAddress :: string()} | {error, any()}.
get_port_mapping(#nat_upnp{ip=Ip, service_url=Url}, Protocol0, ExternalPort, HttpcProfile) ->
    Protocol = protocol(Protocol0),
    Msg = "<u:GetSpecificPortMappingEntry xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++ integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "</u:GetSpecificPortMappingEntry>",
    {ok, IAddr} = inet:parse_address(Ip),
    ok = httpc:set_option(socket_opts, [{ip, IAddr}], HttpcProfile),
    Result =
        case nat_lib:soap_request(Url, "GetSpecificPortMappingEntry", Msg, [], HttpcProfile) of
            {ok, Body} ->
                {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
                [Infos | _] = xmerl_xpath:string("//s:Envelope/s:Body/"
                                                 "u:GetSpecificPortMappingEntryResponse", Xml),
                NewInternalPort =
                extract_txt(
                  xmerl_xpath:string("NewInternalPort/text()",
                                     Infos)
                 ),

                NewInternalClient =
                extract_txt(
                  xmerl_xpath:string("NewInternalClient/text()",
                                     Infos)
                 ),

                {IPort, _ } = string:to_integer(NewInternalPort),
                {ok, IPort, NewInternalClient};
            Error ->
                Error
        end,
    ok = httpc:set_option(socket_opts, [], HttpcProfile),
    Result.



%% @doc get router status
-spec status_info(Context :: nat:nat_upnp(), HttpcProfile :: pid())
-> {Status::string(), LastConnectionError::string(), Uptime::string()}
   | {error, term()}.
status_info(#nat_upnp{service_url=Url}, HttpcProfile) ->
    Message = "<u:GetStatusInfo xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "</u:GetStatusInfo>",
    case nat_lib:soap_request(Url, "GetStatusInfo", Message, HttpcProfile) of
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

get_service_url(RootUrl, HttpcProfile) ->
    case nat_lib:http_get(RootUrl, HttpcProfile) of
        {ok, {{_, 200, _}, _, Body}} ->
            {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
            [Device | _] = xmerl_xpath:string("//device", Xml),
            case device_type(Device) of
                "urn:schemas-upnp-org:device:InternetGatewayDevice:2" ->
                    get_wan_device(Device, RootUrl);
                _ ->
                    {error,  no_gateway_device}
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            {error, integer_to_list(StatusCode)};
        Error ->
            Error
    end.

get_natrsipstatus(Url, HttpcProfile) ->
  Message = "<u:GetNATRSIPStatus xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "</u:GetNATRSIPStatus>",
    case nat_lib:soap_request(Url, "GetNATRSIPStatus", Message, HttpcProfile) of
        {ok, Body} ->
             {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),

            [Infos | _] = xmerl_xpath:string("//s:Envelope/s:Body/"
                                             "u:GetNATRSIPStatusResponse", Xml),
            Enabled = extract_txt(
                       xmerl_xpath:string("NewNATEnabled/text()",
                                          Infos)
                      ),
            case Enabled of
                "1" -> enabled;
                "0" -> disabled
            end;
        Error ->
            Error
    end.





get_wan_device(D, RootUrl) ->
    case get_device(D, "urn:schemas-upnp-org:device:WANDevice:2") of
        {ok, D1} ->
            get_connection_device(D1, RootUrl);
        _ ->
            {erro, no_wan_device}
    end.

get_connection_device(D, RootUrl) ->
    case get_device(D, "urn:schemas-upnp-org:device:WANConnectionDevice:2") of
        {ok, D1} ->
            get_connection_url(D1, RootUrl);

        _ ->
            {error, no_wanconnection_device}
    end.


get_connection_url(D, RootUrl) ->
    case get_service(D, "urn:schemas-upnp-org:service:WANIPConnection:2") of
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
        false -> erlang:error(bad_protocol)
    end,
    string:to_upper(atom_to_list(Protocol)).
