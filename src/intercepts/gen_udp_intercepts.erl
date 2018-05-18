%% @hidden

-module(gen_udp_intercepts).

-export([
    send/4
]).

-define(MODULE_ORI, gen_udp_orig).
-define(LOG(A, B), io:format("[~p] " ++ A ++ "~n", [?MODULE] ++ B)).
-define(KEY(X), {gen_udp, send, X}).

send(Socket, Ip, Port, Msg) ->
    Key = ?KEY([Ip, Port, Msg]),

    dbg:tracer(process, {fun handle_trace/2, Key}),
    dbg:p(self(), [r]),

    ?LOG("[REQ] ~p", [Key]),
    Result = case nat_cache:get(Key) of
        {Socket2, Ip2, Port2, Msg2}=Cached ->
            ?LOG("[RESP] [CACHE] ~p", [Cached]),
            self() ! {udp, Socket2, Ip2, Port2, Msg2},
            ok;
        undefined ->
            ?MODULE_ORI:send_orig(Socket, Ip, Port, Msg)
    end,
    dbg:stop_clear(),
    Result.

handle_trace({trace, _Pid, 'receive', {udp, Socket, Ip, Port, Msg}=Data}, Key) ->
    ?LOG("[RESP] [ORI] ~p", [Data]),
    nat_cache:put(Key, {Socket, Ip, Port, Msg});
handle_trace(_Trace, _Key) ->
    ok.
