-module(part1).
-export([start/0]).

start() ->
	Adapters = day10:adapters(day10:input()),
	DiffCounts = day10:adapt(0, Adapters),
	io:format("Result: ~p~n", [maps:get(1, DiffCounts) * maps:get(3, DiffCounts)]).
