%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.

-module(nat_test_helper).

-export([setup_mocks/0, teardown_mocks/0]).
-export([mock_hackney_get/2, mock_hackney_post/3]).
-export([mock_gen_udp/2]).

%% @doc Setup meck mocks for testing
setup_mocks() ->
    meck:new(hackney, [passthrough, no_link]),
    meck:new(gen_udp, [unstick, passthrough, no_link]),
    ok.

%% @doc Teardown meck mocks
teardown_mocks() ->
    catch meck:unload(hackney),
    catch meck:unload(gen_udp),
    ok.

%% @doc Mock hackney GET request
mock_hackney_get(UrlPattern, Response) ->
    meck:expect(hackney, request,
        fun(get, Url, _Headers, _Body, _Opts) when Url =:= UrlPattern ->
            Response;
           (Method, Url, Headers, Body, Opts) ->
            meck:passthrough([Method, Url, Headers, Body, Opts])
        end).

%% @doc Mock hackney POST request (SOAP)
mock_hackney_post(UrlPattern, _Action, Response) ->
    meck:expect(hackney, request,
        fun(post, Url, _Headers, _Body, _Opts) when Url =:= UrlPattern ->
            Response;
           (Method, Url, Headers, Body, Opts) ->
            meck:passthrough([Method, Url, Headers, Body, Opts])
        end).

%% @doc Mock gen_udp for NAT-PMP/PCP
mock_gen_udp(GatewayIp, ResponseBinary) ->
    meck:expect(gen_udp, send,
        fun(Socket, Ip, Port, _Msg) when Ip =:= GatewayIp ->
            %% Simulate response arriving
            self() ! {udp, Socket, Ip, Port, ResponseBinary},
            ok;
           (Socket, Ip, Port, Msg) ->
            meck:passthrough([Socket, Ip, Port, Msg])
        end).
