-module(part1).
-export([start/0]).

start() ->
	{Preamble, Rest} = day09:parse(day09:input(), 25),
	io:format("Result: ~p~n", [day09:find_no_sum(Preamble, Rest)]).
