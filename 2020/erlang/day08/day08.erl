-module(day08).
-export([input/0, parse_instruction/1, parse_arg/1, parse_program/1, run/1, modified/2, modified_program_options/1]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:chomp(Line)] ++ input()
		   end.

parse_instruction(Instruction) ->
	[Op, Arg] = string:lexemes(Instruction, " "),
	{list_to_atom(Op), parse_arg(Arg)}.

parse_arg([$+|Tail]) -> list_to_integer(Tail);
parse_arg([$-|Tail]) -> -list_to_integer(Tail).

parse_program(Input) -> lists:map(fun parse_instruction/1, Input).

run(Program) -> run(Program, 1, 0, sets:new()).
run(Program, Pc, Acc, _) when Pc > length(Program) -> {Pc, Acc, true};
run(Program, Pc, Acc, AlreadyRun) ->
	case sets:is_element(Pc, AlreadyRun) of
		true -> {Pc, Acc, false};
		_ -> 
			{PcDelta, AccDelta} = execute_instruction(lists:nth(Pc, Program)),
			run(Program, Pc + PcDelta, Acc + AccDelta, sets:add_element(Pc, AlreadyRun))
	end.

execute_instruction({Op, Arg}) ->
	case Op of
		nop -> {1, 0};
		acc -> {1, Arg};
		jmp -> {Arg, 0}
	end.

modified(Index, Program) ->
	{Start, End} = lists:split(Index-1, Program),
	Start ++ [
			  case lists:nth(Index, Program) of
				  {nop, Arg} -> {jmp, Arg};
				  {jmp, Arg} -> {nop, Arg};
				  Instruction -> Instruction
			  end
			 ] ++ tl(End).

modified_program_options(Program) ->
	Indices = lists:seq(1, length(Program)),
	lists:map(fun (Index) -> modified(Index, Program) end, Indices).
