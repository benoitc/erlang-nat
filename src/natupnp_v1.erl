%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

%% @doc Client for UPnP Device Control Protocol Internet Gateway Device v1.
%%
%% documented in detail at: http://upnp.org/specs/gw/UPnP-gw-InternetGatewayDevice-v1-Device.pdf

-module(natupnp_v1).

-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).
-export([get_port_mapping/3]).
-export([get_port_mapping_count/1]).
-export([list_port_mappings/1, list_port_mappings/2]).
-export([get_generic_port_mapping_entry/2]).
-export([status_info/1]).
-export([get_wan_device/2]).

%% Exported for testing
-export([parse_port_mapping_entry/1]).

-include("nat.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(ST, <<"urn:schemas-upnp-org:device:InternetGatewayDevice:1" >>).

%% @doc discover the gateway and our IP to associate
-spec discover() -> {ok, Context:: nat:nat_upnp()}
                    | {error, term()}.
discover() ->
    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),

    MSearch = [<<"M-SEARCH * HTTP/1.1\r\n"
                 "HOST: 239.255.255.250:1900\r\n"
                 "MAN: \"ssdp:discover\"\r\n"
                 "ST: ">>,  ?ST, <<"\r\n"
                                  "MX: 3"
                                  "\r\n\r\n">>],

    try
        discover1(Sock, iolist_to_binary(MSearch), 3)
    after
        gen_udp:close(Sock)
    end.


discover1(_Sock, _MSearch, ?NAT_TRIES) ->
    {error, timeout};
discover1(Sock, MSearch, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = ?NAT_INITIAL_MS bsl Tries,
    ok = gen_udp:send(Sock, "239.255.255.250", 1900, MSearch),
    case discover_loop(Sock, Timeout) of
      {ok, Ip, Location} ->
        case get_service_url(binary_to_list(Location)) of
          {ok, Url} ->
            MyIp = inet_ext:get_internal_address(Ip),
            {ok, #nat_upnp{service_url=Url, ip=MyIp}};
          Error ->
            Error
        end;
      error ->
        discover1(Sock, MSearch, Tries+1)
    end.

discover_loop(Sock, Timeout) ->
    receive
        {udp, Sock, Ip, _Port, Packet} ->
            Headers = nat_lib:get_headers(Packet),
            case maps:find(<<"St">>, Headers) of
              {ok, ?ST} ->
                case maps:find('Location', Headers) of
                  {ok, Location} ->
                    {ok, Ip, Location};
                  error ->
                    error
                end;
              _ ->
                discover_loop(Sock, Timeout)
            end
    after Timeout ->
            error
    end.


get_device_address(#nat_upnp{service_url=Url}) ->
    Res = case uri_string:parse(Url) of
        #{host := Host} ->
            inet:getaddr(Host, inet);
        {error, _, _} = Error ->
            Error
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
                                             "*[local-name() = 'GetExternalIPAddressResponse']", Xml),

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


%% @doc Add a port mapping with default lifetime to 0 seconds
-spec add_port_mapping(nat:nat_upnp(), nat:nat_protocol(), integer(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} | {error, any()}.
add_port_mapping(Context, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Context, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

%% @doc Add a port mapping and release after Timeout
-spec add_port_mapping(nat:nat_upnp(), nat:nat_protocol(),integer(), integer(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} | {error, any()}.
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


add_port_mapping1(#nat_upnp{ip=Ip, service_url=Url}=NatCtx,
                  Protocol, InternalPort, ExternalPort,
                  Lifetime) when is_integer(Lifetime), Lifetime >= 0 ->
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
    {ok, IAddr} = inet:parse_address(Ip),
    Start = nat_lib:timestamp(),
    case nat_lib:soap_request(Url, "AddPortMapping", Msg, [{socket_opts, [{ip, IAddr}]}]) of
        {ok, _} ->
            Now = nat_lib:timestamp(),
            MappingLifetime = if
                                Lifetime > 0 ->
                                  Lifetime - (Now - Start);
                                true ->
                                  infinity
                              end,
            {ok, Now, InternalPort, ExternalPort, MappingLifetime};
        Error when Lifetime > 0 ->
            %% Try to repair error code 725 - OnlyPermanentLeasesSupported
            case only_permanent_lease_supported(Error) of
                true ->
                    error_logger:info_msg("UPNP: only permanent lease supported~n", []),
                    add_port_mapping1(NatCtx, Protocol, InternalPort, ExternalPort, 0);
                false ->
                    Error
              end;
        Error ->
            Error
    end.

only_permanent_lease_supported({error, {http_error, "500", Body}}) ->
  {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
  [Error | _] = xmerl_xpath:string("//s:Envelope/s:Body/s:Fault/detail/"
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
                          ExternalPort :: integer())
-> ok | {error, term()}.
delete_port_mapping(#nat_upnp{ip=Ip, service_url=Url}, Protocol0, _InternalPort, ExternalPort) ->
    Protocol = protocol(Protocol0),
    Msg = "<u:DeletePortMapping xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++ integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "</u:DeletePortMapping>",
    {ok, IAddr} = inet:parse_address(Ip),
    case nat_lib:soap_request(Url, "DeletePortMapping", Msg, [{socket_opts, [{ip, IAddr}]}]) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc get specific port mapping for a well known port and protocol
-spec get_port_mapping(Context :: nat:nat_upnp(),
                       Protocol :: nat:nat_protocol(),
                       ExternalPort :: integer())
-> {ok, InternalPort :: integer(), InternalAddress :: string()} | {error, any()}.
get_port_mapping(#nat_upnp{ip=Ip, service_url=Url}, Protocol0, ExternalPort) ->
   Protocol = protocol(Protocol0),
    Msg = "<u:GetSpecificPortMappingEntry xmlns:u=\""
    "urn:schemas-upnp-org:service:WANIPConnection:1\">"
    "<NewRemoteHost></NewRemoteHost>"
    "<NewExternalPort>" ++ integer_to_list(ExternalPort) ++
    "</NewExternalPort>"
    "<NewProtocol>" ++ Protocol ++ "</NewProtocol>"
    "</u:GetSpecificPortMappingEntry>",
    {ok, IAddr} = inet:parse_address(Ip),
    case nat_lib:soap_request(Url, "GetSpecificPortMappingEntry", Msg, [{socket_opts, [{ip, IAddr}]}]) of
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
    end.


%% @doc get router status
-spec status_info(Context :: nat:nat_upnp())
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


%% @doc Get total number of port mappings
-spec get_port_mapping_count(Context) -> {ok, Count} | {error, Reason} when
    Context :: #nat_upnp{},
    Count :: non_neg_integer(),
    Reason :: term().
get_port_mapping_count(#nat_upnp{service_url=Url}) ->
    Message = "<u:GetPortMappingNumberOfEntries xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "</u:GetPortMappingNumberOfEntries>",
    case nat_lib:soap_request(Url, "GetPortMappingNumberOfEntries", Message) of
        {ok, Body} ->
            {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
            case xmerl_xpath:string("//NewPortMappingNumberOfEntries/text()", Xml) of
                [Resp | _] ->
                    {ok, list_to_integer(extract_txt([Resp]))};
                [] ->
                    {error, no_count_in_response}
            end;
        Error ->
            Error
    end.

%% @doc List all port mappings (with default limit of 100)
-spec list_port_mappings(Context) -> {ok, [#port_mapping{}]} | {error, Reason} when
    Context :: #nat_upnp{},
    Reason :: term().
list_port_mappings(Ctx) ->
    list_port_mappings(Ctx, 100).

%% @doc List all port mappings up to Limit
-spec list_port_mappings(Context, Limit) -> {ok, [#port_mapping{}]} | {error, Reason} when
    Context :: #nat_upnp{},
    Limit :: non_neg_integer(),
    Reason :: term().
list_port_mappings(Ctx, Limit) ->
    list_port_mappings_loop(Ctx, 0, Limit, []).

list_port_mappings_loop(_Ctx, Index, Limit, Acc) when Index >= Limit ->
    {ok, lists:reverse(Acc)};
list_port_mappings_loop(Ctx, Index, Limit, Acc) ->
    case get_generic_port_mapping_entry(Ctx, Index) of
        {ok, Mapping} ->
            list_port_mappings_loop(Ctx, Index + 1, Limit, [Mapping | Acc]);
        {error, {http_error, "714", _}} ->  %% SpecifiedArrayIndexInvalid
            {ok, lists:reverse(Acc)};
        {error, {http_error, "713", _}} ->  %% SpecifiedArrayIndexInvalid (some routers)
            {ok, lists:reverse(Acc)};
        Error ->
            Error
    end.

%% @doc Get a single port mapping by index
-spec get_generic_port_mapping_entry(Context, Index) -> {ok, #port_mapping{}} | {error, Reason} when
    Context :: #nat_upnp{},
    Index :: non_neg_integer(),
    Reason :: term().
get_generic_port_mapping_entry(#nat_upnp{service_url=Url}, Index) ->
    Message = "<u:GetGenericPortMappingEntry xmlns:u=\""
        "urn:schemas-upnp-org:service:WANIPConnection:1\">"
        "<NewPortMappingIndex>" ++ integer_to_list(Index) ++ "</NewPortMappingIndex>"
        "</u:GetGenericPortMappingEntry>",
    case nat_lib:soap_request(Url, "GetGenericPortMappingEntry", Message) of
        {ok, Body} ->
            parse_port_mapping_entry(Body);
        Error ->
            Error
    end.

parse_port_mapping_entry(Body) ->
    {Xml, _} = xmerl_scan:string(Body, [{space, normalize}]),
    case xmerl_xpath:string("//s:Envelope/s:Body/"
                            "*[local-name() = 'GetGenericPortMappingEntryResponse']", Xml) of
        [Infos | _] ->
            RemoteHost = safe_extract_txt(xmerl_xpath:string("NewRemoteHost/text()", Infos), ""),
            ExternalPort = safe_extract_int(xmerl_xpath:string("NewExternalPort/text()", Infos), 0),
            Protocol = safe_extract_protocol(xmerl_xpath:string("NewProtocol/text()", Infos)),
            InternalPort = safe_extract_int(xmerl_xpath:string("NewInternalPort/text()", Infos), 0),
            InternalClient = safe_extract_txt(xmerl_xpath:string("NewInternalClient/text()", Infos), ""),
            Enabled = safe_extract_bool(xmerl_xpath:string("NewEnabled/text()", Infos)),
            Description = safe_extract_txt(xmerl_xpath:string("NewPortMappingDescription/text()", Infos), ""),
            LeaseDuration = safe_extract_int(xmerl_xpath:string("NewLeaseDuration/text()", Infos), 0),

            {ok, #port_mapping{
                remote_host = RemoteHost,
                external_port = ExternalPort,
                protocol = Protocol,
                internal_port = InternalPort,
                internal_client = InternalClient,
                enabled = Enabled,
                description = Description,
                lease_duration = LeaseDuration
            }};
        [] ->
            {error, invalid_response}
    end.

safe_extract_txt([], Default) -> Default;
safe_extract_txt(Xml, _Default) -> extract_txt(Xml).

safe_extract_int([], Default) -> Default;
safe_extract_int(Xml, _Default) ->
    case string:to_integer(extract_txt(Xml)) of
        {Int, _} when is_integer(Int) -> Int;
        _ -> 0
    end.

safe_extract_bool([]) -> false;
safe_extract_bool(Xml) ->
    case extract_txt(Xml) of
        "1" -> true;
        "true" -> true;
        _ -> false
    end.

safe_extract_protocol([]) -> tcp;
safe_extract_protocol(Xml) ->
    case string:to_lower(extract_txt(Xml)) of
        "udp" -> udp;
        _ -> tcp
    end.


%% internals

get_service_url(RootUrl) ->
    case hackney:request(get, RootUrl, [], <<>>, [with_body]) of
        {ok, 200, _RespHeaders, Body} ->
            {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{space, normalize}]),
            [Device | _] = xmerl_xpath:string("//device", Xml),
            case device_type(Device) of
                "urn:schemas-upnp-org:device:InternetGatewayDevice:1" ->
                    get_wan_device(Device, RootUrl);
                _ ->
                    {error,  no_gateway_device}
            end;
        {ok, StatusCode, _RespHeaders, _Body} ->
            {error, integer_to_list(StatusCode)};
        {error, Reason} ->
            {error, Reason}
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
