-module(gen_udp_intercepts).

-export([
    send/4
]).

-define(MODULE_ORI, gen_udp_orig).
-define(LOG(A, B), io:format("[~p] " ++ A ++ "~n", [?MODULE] ++ B)).
-define(KEY(X), {gen_udp, send, X}).

send(Socket, Ip, Port, Msg) ->
    Key = ?KEY([Ip, Port, Msg]),
    ?LOG("[REQ] ~p", [Key]),
    case nat_cache:get(Key) of
        {Socket2, Ip2, Port2, Msg2}=Cached ->
            ?LOG("[RESP] [CACHE] ~p", [Cached]),
            self() ! {udp, Socket2, Ip2, Port2, Msg2},
            ok;
        undefined ->
            Return = ?MODULE_ORI:send_orig(Socket, Ip, Port, Msg),
            receive
                {udp, Socket2, Ip2, Port2, Msg2}=Data ->
                    ?LOG("[RESP] [ORI] ~p", [Data]),
                    nat_cache:put(Key, {Socket2, Ip2, Port2, Msg2}),
                    self() ! {udp, Socket2, Ip2, Port2, Msg2},
                    Return
            after 1000 ->
                ?LOG("[RESP] [ORI] ~p", [timeout]),
                timeout
            end
    end.
