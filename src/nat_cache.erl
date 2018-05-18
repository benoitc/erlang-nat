%%%-------------------------------------------------------------------
%% @private
%% @doc
%% == NAT Debug ==
%% @end
%%%-------------------------------------------------------------------
-module(nat_cache).

-behavior(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start/1, start_link/1, stop/0
    ,get/1
    ,put/2
    ,print/0
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1
    ,handle_call/3
    ,handle_cast/2
    ,handle_info/2
    ,terminate/2
    ,code_change/3
]).

-define(SERVER, ?MODULE).
-define(TABLE, intercept_cache).

-record(state, {
    dets
    ,get = true
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Args) ->
    gen_server:start({local, ?SERVER}, ?SERVER, Args, []).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get(any()) -> any().
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put(any(), any()) -> ok.
put(Key, Value) ->
    gen_server:cast(?SERVER, {put, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec print() -> ok.
print() ->
    gen_server:cast(?SERVER, print).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
    Get = proplists:get_value(get, Args, true),
    File = proplists:get_value(file, Args, "nat_scan"),
    {ok, Dets} = dets:open_file(?TABLE, [{file, File}]),
    {ok, #state{dets=Dets, get=Get}}.

handle_call({get, _Key}, _From, #state{get=false}=State) ->
    {reply, undefined, State};
handle_call({get, Key}, _From, #state{dets=Dets}=State) ->
    Reply =
        case dets:lookup(Dets, Key) of
            {error, _R} -> undefined;
            [] -> undefined;
            [{_, V}|_] -> V
        end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({put, Key, Value}, #state{dets=Dets}=State) ->
    ok = dets:insert(Dets, {Key, Value}),
    {noreply,  State};
handle_cast(print, #state{dets=Dets}=State) ->
    dets:foldl(
        fun(Obj, _Acc) ->
            io:format("~p~n", [Obj])
        end
        ,ok
        ,Dets
    ),
    {noreply,  State};
handle_cast(_Msg, State) ->
    {noreply,  State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{dets=Dets}) ->
    ok = dets:close(Dets).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
