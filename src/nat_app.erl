%%% -*- erlang -*-
%%% This file is part of erlang-nat released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2016-2024 Benoît Chesneau <benoitc@refuge.io>

-module(nat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nat_sup:start_link().

stop(_State) ->
    ok.
