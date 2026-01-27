%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2024 Benoît Chesneau <benoitc@refuge.io>

%%% @doc Port Control Protocol (RFC 6887)
%%% Successor to NAT-PMP, supports IPv6

-module(natpcp).

-export([discover/0]).
-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).

%% Exported for testing
-export([ip_to_pcp_binary/1, pcp_binary_to_ip/1, parse_response/2]).

-include("nat.hrl").

-define(PCP_PORT, 5351).  %% Same as NAT-PMP
-define(PCP_VERSION, 2).

%% Opcodes
-define(OP_ANNOUNCE, 0).
-define(OP_MAP, 1).
-define(OP_PEER, 2).

%% Result codes
-define(SUCCESS, 0).
-define(UNSUPP_VERSION, 1).
-define(NOT_AUTHORIZED, 2).
-define(MALFORMED_REQUEST, 3).
-define(UNSUPP_OPCODE, 4).
-define(UNSUPP_OPTION, 5).
-define(MALFORMED_OPTION, 6).
-define(NETWORK_FAILURE, 7).
-define(NO_RESOURCES, 8).
-define(UNSUPP_PROTOCOL, 9).
-define(USER_EX_QUOTA, 10).
-define(CANNOT_PROVIDE_EXTERNAL, 11).
-define(ADDRESS_MISMATCH, 12).
-define(EXCESSIVE_REMOTE_PEERS, 13).

%% Protocol numbers
-define(PROTO_TCP, 6).
-define(PROTO_UDP, 17).


-type pcp_error() :: unsupp_version
                   | not_authorized
                   | malformed_request
                   | unsupp_opcode
                   | unsupp_option
                   | malformed_option
                   | network_failure
                   | no_resources
                   | unsupp_protocol
                   | user_ex_quota
                   | cannot_provide_external
                   | address_mismatch
                   | excessive_remote_peers
                   | bad_response
                   | timeout.

-export_type([pcp_error/0]).

%% @doc get the gateway address
-spec get_device_address(Gateway) -> {ok, Ip} | {error, Reason} when
    Gateway :: inet:ip_address() | inet:hostname(),
    Ip :: inet:ip_address() | inet:hostname(),
    Reason :: pcp_error().
get_device_address(Gateway) ->
    {ok, Gateway}.

%% @doc get external ip
-spec get_external_address(Gateway) -> {ok, ExternalIp} | {error, Reason} when
    Gateway :: inet:ip_address() | inet:hostname(),
    ExternalIp :: string(),
    Reason :: pcp_error().
get_external_address(Gateway) ->
    %% PCP doesn't have a direct "get external address" like NAT-PMP
    %% We send an ANNOUNCE to check if the gateway supports PCP
    %% The actual external address is returned in MAP responses
    case send_announce(Gateway) of
        {ok, _Epoch} ->
            %% For compatibility, we do a MAP with 0 lifetime to get external address
            case do_map_request(Gateway, tcp, 0, 0, 0) of
                {ok, ExternalIp, _, _, _} ->
                    {ok, inet:ntoa(ExternalIp)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc get internal address used for this gateway
-spec get_internal_address(Gateway) -> {ok, InternalIp} when
    Gateway :: inet:ip_address() | inet:hostname(),
    InternalIp :: inet:ip_address() | inet:hostname().
get_internal_address(Gateway) ->
    {ok, inet_ext:get_internal_address(Gateway)}.

%% @doc discover a PCP-capable gateway
-spec discover() -> {ok, Gateway} | {error, any()} when
      Gateway :: inet:ip_address().
discover() ->
    IPs = case natpmp:system_gateways() of
              [] ->  natpmp:potential_gateways();
              Gateways -> Gateways
          end,

    Ref = make_ref(),
    Self = self(),

    Workers = lists:foldl(fun(Ip, Acc) ->
                                  Pid = spawn_link(fun() ->
                                                           discover_with_addr(Self, Ref, Ip)
                                                   end),
                                  erlang:monitor(process, Pid),
                                  [Pid | Acc]
                          end, [], lists:usort(IPs)),

    discover_wait(Workers, Ref).

discover_with_addr(Parent, Ref, Addr) ->
    case send_announce(Addr) of
        {ok, _Epoch} ->
            Parent ! {nat, Ref, self(), Addr};
        _Else ->
            ok
    end.

discover_wait([], _Ref) ->
    {error, no_nat};
discover_wait(Workers, Ref) ->
    receive
        {nat, Ref, WorkerPid, GatewayIp} ->
            lists:foreach(fun(Pid) ->
                                  catch unlink(Pid),
                                  catch exit(Pid, shutdown),
                                  receive
                                      {'DOWN', _, _, Pid, _} -> ok
                                  end
                          end, Workers -- [WorkerPid]),
            {ok, GatewayIp};
        {'DOWN', _MRef, _Type, WorkerPid, _Info} ->
            discover_wait(Workers -- [WorkerPid], Ref)
    end.

%% @doc add a port mapping with default lifetime
-spec add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer(),
      Reason :: pcp_error().
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

%% @doc add a port mapping
-spec add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest, Lifetime) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Lifetime :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer(),
      Reason :: pcp_error().
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort, Lifetime) ->
    case do_map_request(Gateway, Protocol, InternalPort, ExternalPort, Lifetime) of
        {ok, _ExternalIp, Since, ExtPort, MappingLifetime} ->
            {ok, Since, InternalPort, ExtPort, MappingLifetime};
        Error ->
            Error
    end.

%% @doc delete a port mapping
-spec delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) ->
    ok | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Reason :: pcp_error().
delete_port_mapping(Gateway, Protocol, InternalPort, _ExternalPort) ->
    %% RFC 6887: To delete a mapping, send a MAP request with lifetime=0
    case do_map_request(Gateway, Protocol, InternalPort, 0, 0) of
        {ok, _, _, _, 0} -> ok;
        {ok, _, _, _, _} -> {error, bad_response};
        Error -> Error
    end.


%% ---------------------
%% - private functions -
%% ---------------------

%% Send an ANNOUNCE request to verify PCP support
send_announce(Gateway0) ->
    Gateway = inet_ext:parse_address(Gateway0),
    InternalAddrStr = inet_ext:get_internal_address(Gateway),
    {ok, InternalAddr} = inet:parse_address(InternalAddrStr),
    InternalAddrBin = ip_to_pcp_binary(InternalAddr),

    %% ANNOUNCE request: Version=2, Opcode=0, Reserved=0, Lifetime=0, Client IP
    Request = <<?PCP_VERSION:8,
                ?OP_ANNOUNCE:8,
                0:16,           %% Reserved
                0:32,           %% Requested lifetime (0 for ANNOUNCE)
                InternalAddrBin/binary>>,

    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),
    try
        pcp_rpc(Sock, Gateway, Request, ?OP_ANNOUNCE)
    after
        gen_udp:close(Sock)
    end.

%% Send a MAP request
do_map_request(Gateway0, Protocol, InternalPort, ExternalPort, Lifetime) ->
    Gateway = inet_ext:parse_address(Gateway0),
    InternalAddrStr = inet_ext:get_internal_address(Gateway),
    {ok, InternalAddr} = inet:parse_address(InternalAddrStr),
    InternalAddrBin = ip_to_pcp_binary(InternalAddr),

    ProtoNum = case Protocol of
                   tcp -> ?PROTO_TCP;
                   udp -> ?PROTO_UDP
               end,

    %% Generate random nonce
    Nonce = crypto:strong_rand_bytes(12),

    %% Build MAP request (24 bytes header + 36 bytes MAP payload)
    Request = <<?PCP_VERSION:8,
                ?OP_MAP:8,
                0:16,                       %% Reserved
                Lifetime:32,                %% Requested lifetime
                InternalAddrBin/binary,     %% Client IP (16 bytes)
                %% MAP payload:
                Nonce/binary,               %% Mapping nonce (12 bytes)
                ProtoNum:8,                 %% Protocol
                0:24,                       %% Reserved
                InternalPort:16,            %% Internal port
                ExternalPort:16,            %% Suggested external port
                0:128>>,                    %% Suggested external address (0 = any)

    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),
    try
        pcp_rpc(Sock, Gateway, Request, ?OP_MAP)
    after
        gen_udp:close(Sock)
    end.

pcp_rpc(Sock, Gateway, Request, OpCode) ->
    pcp_rpc1(Sock, Gateway, Request, OpCode, 0).

pcp_rpc1(_Sock, _Gateway, _Request, _OpCode, ?NAT_TRIES) ->
    {error, timeout};
pcp_rpc1(Sock, Gateway, Request, OpCode, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = ?NAT_INITIAL_MS bsl Tries,
    case gen_udp:send(Sock, Gateway, ?PCP_PORT, Request) of
        ok ->
            receive
                {udp, _Sock, Gateway, _Port, Packet} ->
                    parse_response(Packet, OpCode);
                {udp, _, _, _, _} ->
                    pcp_rpc1(Sock, Gateway, Request, OpCode, Tries + 1)
            after Timeout ->
                      pcp_rpc1(Sock, Gateway, Request, OpCode, Tries + 1)
            end;
        _Error ->
            pcp_rpc1(Sock, Gateway, Request, OpCode, Tries + 1)
    end.

%% Parse ANNOUNCE response
parse_response(<<_Version:8, RespOpCode:8, _Reserved:8, ResultCode:8,
                 _Lifetime:32, Epoch:32, _Reserved2:96>>, ?OP_ANNOUNCE)
  when RespOpCode =:= (128 + ?OP_ANNOUNCE) ->
    case parse_result_code(ResultCode) of
        ok -> {ok, Epoch};
        Error -> Error
    end;

%% Parse MAP response
parse_response(<<_Version:8, RespOpCode:8, _Reserved:8, ResultCode:8,
                 Lifetime:32, Epoch:32, _Reserved2:96,
                 _Nonce:12/binary,
                 _Protocol:8, _Reserved3:24,
                 _InternalPort:16, ExternalPort:16,
                 ExternalAddr:16/binary>>, ?OP_MAP)
  when RespOpCode =:= (128 + ?OP_MAP) ->
    case parse_result_code(ResultCode) of
        ok ->
            ExternalIp = pcp_binary_to_ip(ExternalAddr),
            {ok, ExternalIp, Epoch, ExternalPort, Lifetime};
        Error ->
            Error
    end;

parse_response(_, _) ->
    {error, bad_response}.

parse_result_code(?SUCCESS) -> ok;
parse_result_code(?UNSUPP_VERSION) -> {error, unsupp_version};
parse_result_code(?NOT_AUTHORIZED) -> {error, not_authorized};
parse_result_code(?MALFORMED_REQUEST) -> {error, malformed_request};
parse_result_code(?UNSUPP_OPCODE) -> {error, unsupp_opcode};
parse_result_code(?UNSUPP_OPTION) -> {error, unsupp_option};
parse_result_code(?MALFORMED_OPTION) -> {error, malformed_option};
parse_result_code(?NETWORK_FAILURE) -> {error, network_failure};
parse_result_code(?NO_RESOURCES) -> {error, no_resources};
parse_result_code(?UNSUPP_PROTOCOL) -> {error, unsupp_protocol};
parse_result_code(?USER_EX_QUOTA) -> {error, user_ex_quota};
parse_result_code(?CANNOT_PROVIDE_EXTERNAL) -> {error, cannot_provide_external};
parse_result_code(?ADDRESS_MISMATCH) -> {error, address_mismatch};
parse_result_code(?EXCESSIVE_REMOTE_PEERS) -> {error, excessive_remote_peers};
parse_result_code(_) -> {error, unknown_error}.

%% Convert IPv4 or IPv6 address to PCP 16-byte format
%% IPv4 addresses are encoded as IPv4-mapped IPv6 addresses (::ffff:a.b.c.d)
ip_to_pcp_binary({A, B, C, D}) ->
    %% IPv4 -> IPv4-mapped IPv6
    <<0:80, 16#ffff:16, A:8, B:8, C:8, D:8>>;
ip_to_pcp_binary({A, B, C, D, E, F, G, H}) ->
    %% Already IPv6
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>.

%% Convert PCP 16-byte address to IPv4 or IPv6
pcp_binary_to_ip(<<0:80, 16#ffff:16, A:8, B:8, C:8, D:8>>) ->
    %% IPv4-mapped IPv6 -> IPv4
    {A, B, C, D};
pcp_binary_to_ip(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    %% IPv6
    {A, B, C, D, E, F, G, H}.
