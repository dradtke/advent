-module(day09_tests).
-include_lib("eunit/include/eunit.hrl").

example_input() -> ["35", "20", "15", "25", "47", "40", "62", "55", "65", "95", "102", "117", "150", "182", "127", "219", "299", "277", "309", "576"].

find_no_sum_test_() ->
	{Preamble, Rest} = day09:parse(example_input(), 5),
	?_assertEqual(127, day09:find_no_sum(Preamble, Rest)).

fund_sum_set_test_() ->
	[?_assertEqual([15, 25, 47, 40], day09:find_sum_set(day09:parse(example_input()), 127))].
