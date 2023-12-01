-module(aoc).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
    day1:run(),
    {ok, self()}.

stop(_State) ->
    ok.
