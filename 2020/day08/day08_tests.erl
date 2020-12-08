-module(day08_tests).
-include_lib("eunit/include/eunit.hrl").

-export([example_input/0]).

example_input() -> [
	"nop +0",
	"acc +1",
	"jmp +4",
	"acc +3",
	"jmp -3",
	"acc -99",
	"acc +1",
	"jmp -4",
	"acc +6"
				   ].

parse_arg_test_() ->
	[?_assertEqual(0, day08:parse_arg("+0")),
	 ?_assertEqual(4, day08:parse_arg("+4")),
	 ?_assertEqual(-6, day08:parse_arg("-6"))].

parse_instruction_test_() ->
	[?_assertEqual({jmp, 4}, day08:parse_instruction("jmp +4"))].

run_test_() ->
	Program = day08:parse_program(example_input()),
	{_, Acc, _} = day08:run(Program),
	?_assertEqual(5, Acc).

modified_test_() ->
	Program = [{nop, 1}, {acc, 2}, {jmp, 3}],
	[?_assertEqual([{jmp, 1}, {acc, 2}, {jmp, 3}], day08:modified(1, Program)),
	 ?_assertEqual([{nop, 1}, {acc, 2}, {jmp, 3}], day08:modified(2, Program)),
	 ?_assertEqual([{nop, 1}, {acc, 2}, {nop, 3}], day08:modified(3, Program))].
