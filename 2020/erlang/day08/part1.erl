-module(part1).
-export([start/0]).

start() ->
	Program = day08:parse_program(day08:input()),
	{_, Acc, _} = day08:run(Program),
	io:format("Result: ~p~n", [Acc]).
