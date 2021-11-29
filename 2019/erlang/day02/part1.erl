-module(part1).
-export([start/0, interpret/1, show/1, program/1]).

parse(Input) -> [ erlang:list_to_integer(D) || D <- string:lexemes(Input, ",") ].

program(List) -> 
	WithIndex = lists:zip(lists:seq(0, length(List)-1), List),
	maps:from_list(WithIndex).

read_position(Program, Index) -> maps:get(maps:get(Index, Program), Program).
store_position(Program, Index, Value) -> maps:put(maps:get(Index, Program), Value, Program).

interpret(X) ->
	if
		erlang:is_list(X) -> interpret(program(X), 0);
		erlang:is_map(X) -> interpret(X, 0)
	end.
interpret(Program, Index) ->
	case maps:get(Index, Program) of
		1 -> 
			A = read_position(Program, Index + 1),
			B = read_position(Program, Index + 2),
			P = store_position(Program, Index + 3, A + B),
			interpret(P, Index + 4);

		2 -> 
			A = read_position(Program, Index + 1),
			B = read_position(Program, Index + 2),
			P = store_position(Program, Index + 3, A * B),
			interpret(P, Index + 4);

		99 -> Program
	end.

show(Program) ->
	Sorter = fun ({A,_}, {B,_}) -> A < B end,
	[ Value || {_,Value} <- lists:sort(Sorter, maps:to_list(Program)) ].

modify(Program) -> maps:put(2, 2, maps:put(1, 12, Program)).

start() ->
	Input = parse(string:trim(io:get_line(""))),
	ProgramBefore = modify(program(Input)),
	ProgramAfter = interpret(ProgramBefore),
	io:format("~p~n", [maps:get(0, ProgramAfter)]).
