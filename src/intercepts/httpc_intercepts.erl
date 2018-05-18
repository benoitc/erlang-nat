%% @hidden
-module(httpc_intercepts).

-export([
    request/1, request/4
]).

-define(MODULE_ORI, httpc_orig).
-define(CACHE, "data/test.data").
-define(LOG(A, B), io:format("[~p] " ++ A ++ "~n", [?MODULE] ++ B)).
-define(KEY(X), {httpc, request, X}).

request(Url) ->
    Key = ?KEY([Url]),
    ?LOG("[REQ] ~p", [Key]),
    case nat_cache:get(Key) of
        {ok, _}=Result ->
            ?LOG("[RESP] [CACHE] ~p", [Result]),
            Result;
        undefined ->
            Result = ?MODULE_ORI:request_orig(Url),
            ?LOG("[RESP] [ORI] ~p", [Result]),
            nat_cache:put(Key, Result),
            Result
    end.

request(Method, Request, HTTPOptions, Options) ->
    Key = ?KEY([Method, Request, HTTPOptions, Options]),
    ?LOG("[REQ] ~p", [Key]),
    case nat_cache:get(Key) of
        {ok, _}=Result ->
            ?LOG("[RESP] [CACHE] ~p", [Result]),
            Result;
        undefined ->
            Result = ?MODULE_ORI:request_orig(Method, Request, HTTPOptions, Options),
            ?LOG("[RESP] [ORI] ~p", [Result]),
            nat_cache:put(Key, Result),
            Result
    end.
