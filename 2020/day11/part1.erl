-module(part1).
-export([start/0]).

start() ->
	{Map, Width, Height} = day11:parse(day11:input()),
	Result = day11:count_occupied(day11:calculate_changes(Map, Width, Height)),
	io:format("Result: ~p~n", [Result]).
