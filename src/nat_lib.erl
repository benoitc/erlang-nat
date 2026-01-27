%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

-module(nat_lib).

-export([soap_request/3, soap_request/4]).
-export([random_port/0]).
-export([timestamp/0]).
-export([get_headers/1, get_headers/2]).

soap_request(Url, Function, Msg0) ->
  soap_request(Url, Function, Msg0, []).

soap_request(Url, Function, Msg0, Options) ->
    Msg =  "<?xml version=\"1.0\"?>"
           "<s:Envelope"
           " xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\""
           " s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
           "<s:Body>" ++  Msg0 ++ "</s:Body></s:Envelope>",

    Action = "\"urn:schemas-upnp-org:service:WANIPConnection:1#"
             ++ Function ++ "\"",

    Headers = [{<<"Content-Type">>, <<"text/xml; charset=\"utf-8\"">>},
               {<<"User-Agent">>, <<"Darwin/10.0.0, UPnP/1.0, MiniUPnPc/1.3">>},
               {<<"SOAPAction">>, list_to_binary(Action)},
               {<<"Connection">>, <<"close">>},
               {<<"Cache-Control">>, <<"no-cache">>},
               {<<"Pragma">>, <<"no-cache">>}],

    HackneyOpts = case proplists:get_value(socket_opts, Options, []) of
        [] -> [with_body];
        SockOpts ->
            case proplists:get_value(ip, SockOpts) of
                undefined -> [with_body];
                IpAddr -> [{connect_options, [{ip, IpAddr}]}, with_body]
            end
    end,

    case hackney:request(post, Url, Headers, list_to_binary(Msg), HackneyOpts) of
        {ok, 200, _RespHeaders, RespBody} ->
            {ok, binary_to_list(RespBody)};
        {ok, Status, _RespHeaders, RespBody} ->
            error_logger:info_msg("UPNP SOAP error: ~p ~p~n", [Status, RespBody]),
            {error, {http_error, integer_to_list(Status), binary_to_list(RespBody)}};
        {error, Reason} ->
            {error, Reason}
    end.

random_port() ->
    rand:uniform(16#FFFF - 10000) + 10000.

timestamp() ->
    {Mega, Sec, _} = erlang:timestamp(),
    Mega * 1000000 + Sec.

get_headers(Raw) ->
  get_headers(Raw, #{}).

get_headers(Raw, Headers) ->
  case erlang:decode_packet(httph_bin, Raw, []) of
    {ok, {http_error, _}, Rest} ->
      get_headers(Rest, Headers);
    {ok, {http_header, _, H, _, V}, Rest} ->
      get_headers(Rest, Headers#{ H => V });
    _ ->
      Headers
  end.
