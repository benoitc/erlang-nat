%% @hidden
-module(inet_ext_intercepts).

-export([
    get_internal_address/1
]).

-define(MODULE_ORI, inet_ext_orig).
-define(LOG(A, B), io:format("[~p] " ++ A ++ "~n", [?MODULE] ++ B)).
-define(KEY(X), {inet_ext, get_internal_address, X}).

get_internal_address(IP) ->
    Key = ?KEY([IP]),
    ?LOG("[REQ] ~p", [Key]),
    case nat_cache:get(Key) of
        undefined ->
            Return = ?MODULE_ORI:get_internal_address_orig(IP),
            ?LOG("[RESP] [ORI] ~p", [Return]),
            nat_cache:put(Key, Return),
            Return;
        IntAddress ->
            ?LOG("[RESP] [CACHE] ~p", [IntAddress]),
            IntAddress
    end.
