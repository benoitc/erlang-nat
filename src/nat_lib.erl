%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2018 Benoît Chesneau <benoitc@refuge.io>

-module(nat_lib).
-compile(nowarn_deprecated_function).

-export([http_get/2]).
-export([http_request/5]).
-export([soap_request/4, soap_request/5]).
-export([random_port/0]).
-export([timestamp/0]).

http_get(RootUrl, HttpcProfile) ->
    http_request(get, {RootUrl, []}, [{timeout, 5000}], [], HttpcProfile).

http_post(Req, HttpOptions, HttpcProfile) ->
    http_request(post, Req, [{timeout, 5000}], HttpOptions, HttpcProfile).

http_request(Method, Req, ReqOptions, HttpOptions, HttpcProfile) ->
    ReturnPid = self(),
    Pid = proc_lib:spawn(fun() -> ReturnPid ! httpc:request(Method, Req, ReqOptions, HttpOptions, HttpcProfile) end),
    receive
        Response -> Response
    after 7000 ->
        exit(Pid, no_response_from_httpc_client),
        {error, httpc_client_timeout}
    end.

soap_request(Url, Function, Msg0, HttpcProfile) ->
  soap_request(Url, Function, Msg0, [], HttpcProfile).

soap_request(Url, Function, Msg0, Options, HttpcProfile) ->
    Msg =  "<?xml version=\"1.0\"?>"
           "<s:Envelope"
           " xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\""
           " s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">"
           "<s:Body>" ++  Msg0 ++ "</s:Body></s:Envelope>",

    Action = "\"urn:schemas-upnp-org:service:WANIPConnection:1#"
             ++ Function ++ "\"",

    Headers = [{"Content-Length", integer_to_list(length(Msg))},
               {"User-Agent", "Darwin/10.0.0, UPnP/1.0, MiniUPnPc/1.3"},
               {"SOAPAction", Action},
               {"Connection", "close"},
               {"Cache-Control", "no-cache"},
               {"Pragma", "no-cache"}],


    Req = {Url, Headers, "text/xml; charset=\"utf-8\"", Msg},

    case http_post(Req, Options, HttpcProfile) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        OK = {ok, {{_, Status, _}, _, Body}} ->
            error_logger:info_msg("UPNP SOAP error: ~p~n", [OK]),
            {error, {http_error, integer_to_list(Status), Body}};
        Error ->
            Error
    end.

random_port() ->
    random:uniform(16#FFFF - 10000) + 10000.

timestamp() ->
    {Mega,Sec, _} = erlang_ts(),
    Mega*1000000+Sec.

erlang_ts() ->
    try
        erlang:timestamp()
    catch
        error:undef ->
            erlang:now()
    end.
