-module(part2).
-export([start/0]).

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

inputs() -> [{Noun, Verb} || Noun <- lists:seq(0, 99), Verb <- lists:seq(0, 99)].

modify(Program, Noun, Verb) -> maps:put(2, Verb, maps:put(1, Noun, Program)).

search(Program, [{Noun, Verb} | Tail]) ->
	ProgramAfter = interpret(modify(Program, Noun, Verb)),
	Output = maps:get(0, ProgramAfter),
	if
		Output =:= 19690720 -> (100 * Noun) + Verb;
		true -> search(Program, Tail)
	end.


start() ->
	Input = parse(string:trim(io:get_line(""))),
	Program = program(Input),
	Result = search(Program, inputs()),
	io:format("~p~n", [Result]).
