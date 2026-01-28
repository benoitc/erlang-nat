%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2024 Benoît Chesneau <benoitc@refuge.io>

-module(nat_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        #{
            id => nat_server,
            start => {nat_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [nat_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
