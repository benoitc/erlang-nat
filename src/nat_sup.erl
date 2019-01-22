-module(nat_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Workers =
        [{nat, {nat, start_link, []},
          permanent, 5000, worker, [nat]}],
    {ok, {{one_for_one, 1, 5}, Workers}}.
