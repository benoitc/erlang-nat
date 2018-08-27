%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

-module(natpmp).

-export([get_device_address/1]).
-export([get_external_address/1]).
-export([get_internal_address/1]).
-export([discover/0]).
-export([add_port_mapping/4, add_port_mapping/5]).
-export([delete_port_mapping/4]).

-include("nat.hrl").

-define(NAT_PMP_PORT, 5351).


-type natpmp_error() :: unsupported_version
                        | not_authorized
                        | network_failure
                        | out_of_resource
                        | unsupported_opcode
                        | bad_response.

-export_types([natpmp_error/0]).

-spec get_device_address(Gateway) -> {ok, Ip} | {error, Reason} when
	Gateway :: inet:ip_address() | inet:hostname(),
    Ip :: inet:ip_address() | inet:hostname(),
    Reason :: natpmp_error().
get_device_address(Gateway) ->
    {ok, Gateway}.

%% @doc get external ip
-spec get_external_address(Gateway) -> {ok, ExternalIp} | {error, Reason} when
	Gateway :: inet:ip_address() | inet:hostname(),
    ExternalIp :: inet:ip_address() | inet:hostname(),
    Reason :: natpmp_error().
get_external_address(Gateway) ->
	Msg = << 0, 0 >>,
	nat_rpc(Gateway, Msg, 0).

%% @doc get internal address used for this gateway
-spec get_internal_address(Gateway) -> {ok, InternalIp} when
	Gateway :: inet:ip_address() | inet:hostname(),
    InternalIp :: inet:ip_address() | inet:hostname().
get_internal_address(Gateway) ->
    {ok, inet_ext:get_internal_address(Gateway)}.

discover_with_addr(Parent, Ref, Addr) ->
    case (catch natpmp:get_external_address(Addr)) of
        {ok, _Ip} ->
            Parent ! {nat, Ref, self(), Addr};
        _Else ->
            ok
    end.

potential_gateways() ->
    Net_10 = inet_cidr:parse("10.0.0.0/8"),
    Net_172_16 = inet_cidr:parse("172.16.0.0/12"),
    Net_192_168 = inet_cidr:parse("192.168.0.0/16"),
    Networks = [Net_10, Net_172_16, Net_192_168],
    lists:foldl(fun({_, {Addr, Mask}}, Acc) ->
                        case is_network(Networks, Addr) of
                            true ->
                                case inet_cidr:is_ipv4(Addr) of
                                    true ->
                                        Ip0 = mask(Addr, Mask),
                                        Ip = setelement(4, Ip0,
                                                        element(4, Ip0) bor 1),
                                        [Ip | Acc];
                                    false ->
                                        Acc
                                end;
                            false ->
                                Acc
                        end
                end, [], inet_ext:routes()).

system_gateways() ->
    [Ip || {_, Ip} <- inet_ext:gateways()].

%% @doc discover a Nat gateway
-spec discover() -> {ok, Gateway} | {error, any()} when
      Gateway :: inet:ip_address().
discover() ->
    IPs = case system_gateways() of
              [] ->  potential_gateways();
              Gateways -> Gateways
          end,

     Ref = make_ref(),
     Self = self(),

     Workers = lists:foldl(fun(Ip, Acc) ->
                                   Pid = spawn_link(fun() ->
                                                            discover_with_addr(Self, Ref, Ip)
                                                    end),
                                   erlang:monitor(process, Pid),
                                   [Pid | Acc]
                           end, [], lists:usort(IPs)),

     discover_wait(Workers, Ref).

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
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer(),
      Reason :: natpmp_error().
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort) ->
    add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort,
                     ?RECOMMENDED_MAPPING_LIFETIME_SECONDS).

%% @doc add a port mapping
-spec add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest, Lifetime) ->
    {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Lifetime :: non_neg_integer(),
      Since :: non_neg_integer(),
      ExternalPort :: non_neg_integer(),
      MappingLifetime :: non_neg_integer(),
      Reason :: natpmp_error().
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort, Lifetime) ->
    OpCode = case Protocol of
                 udp -> 1;
                 tcp -> 2;
                 _ -> error(unknown_protocol)
             end,

    Msg = << 0,
             OpCode,
             0:16,
             InternalPort:16/unsigned-integer,
             ExternalPort:16/unsigned-integer,
             Lifetime:32/unsigned-integer >>,

    nat_rpc(Gateway, Msg, OpCode).


%% @doc delete a port mapping
-spec delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) ->
    ok | {error, Reason}
      when
      Gateway :: inet:ip_address() | inet:hostname(),
      Protocol :: tcp | udp,
      InternalPort :: non_neg_integer(),
      ExternalPortRequest :: non_neg_integer(),
      Reason :: natpmp_error().
delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPort) ->
	case add_port_mapping(Gateway, Protocol, InternalPort, ExternalPort, 0) of
        {ok, _, InternalPort, 0, 0} -> ok;
        {ok, _, _, _, _} -> {error, bad_response};
        Error -> Error
    end.



%% ---------------------
%% - private functions -
%% ---------------------
%%

nat_rpc(Gateway0, Msg, OpCode) ->
	_ = application:start(inets),
    Gateway = inet_ext:parse_address(Gateway0),
    {ok, Sock} = gen_udp:open(0, [{active, once}, inet, binary]),
    try
        nat_rpc1(Sock, Gateway, Msg, OpCode, 0)
    after
        gen_udp:close(Sock)
    end.


nat_rpc1(_Sock, _Gateway, _Msg, _OpCode, ?NAT_TRIES) ->
    timeout;
nat_rpc1(Sock, Gateway, Msg, OpCode, Tries) ->
    inet:setopts(Sock, [{active, once}]),
    Timeout = ?NAT_INITIAL_MS bsl Tries,
    case gen_udp:send(Sock, Gateway, ?NAT_PMP_PORT, Msg) of
        ok ->
            receive
                {udp, _Sock, Gateway, _Port, Packet} ->
                    parse_response(Packet, OpCode);
                {udp, _, _, _, _} ->
                    nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
            after Timeout ->
                      nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
            end;
        _Error ->
            nat_rpc1(Sock, Gateway, Msg, OpCode, Tries + 1)
    end.




parse_response(<< _Version, ResponseCode, Status:16/unsigned-integer,
                  _Since:32/unsigned-integer, A, B, C, D >>, OpCode) ->

     ExpectedCode = OpCode + 128,
     if
         ExpectedCode =:= ResponseCode ->
            case parse_status(Status) of
                ok -> {ok, inet:ntoa({A, B, C, D})};
                Error -> Error
            end;
         true ->
             {error, bad_response}
     end;
parse_response(<< _Version, ResponseCode, Status:16/unsigned-integer,
                 Since:32/unsigned-integer,
                 InternalPort:16/unsigned-integer,
                 ExternalPort:16/unsigned-integer,
                 Lifetime:32/unsigned-integer >>, OpCode) ->

    ExpectedCode = OpCode + 128,
    if
        ExpectedCode =:= ResponseCode ->
            case parse_status(Status) of
                ok -> {ok, Since, InternalPort, ExternalPort, Lifetime};
                Error -> Error
            end;
        true ->
            {error, bad_response}
    end;
parse_response(_, _) ->
    {error, bad_response}.



parse_status(0) -> ok;
parse_status(1) -> {error, unsupported_version};
parse_status(2) -> {error, not_authorized};
parse_status(3) -> {error, network_failure};
parse_status(4) -> {error, out_of_resource};
parse_status(5) -> {error, unsupported_opcode}.


%% check if an ip is a member of the test networks
is_network([Net | Rest], Addr) ->
    case inet_cidr:contains(Net, Addr) of
        true -> true;
        false -> is_network(Rest, Addr)
    end;
is_network([], _Addr) ->
    false.

%% apply mask to the ip
mask({A, B, C, D}, {E, F, G, H}) ->
    {A band E, B band F, C band G, D band H}.
