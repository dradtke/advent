-module(part2).
-export([start/0]).

start() ->
	Result = day10:count_adapt_all(0, day10:adapters(day10:input())),
	io:format("Result: ~p~n", [Result]).
