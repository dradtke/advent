-module(part1_tests).
-include_lib("eunit/include/eunit.hrl").

parse_line_test_() ->
	[?_assertEqual({policy, 1, 3, $a, "abcde"}, part1:parse_line("1-3 a: abcde"))].

count_char_test_() ->
	[?_assertEqual(3, part1:count_char($a, "aaabcde"))].

is_valid_test_() ->
	[?_assertEqual(true, part1:is_valid({policy, 1, 3, $a, "abcde"})),
	 ?_assertEqual(false, part1:is_valid({policy, 1, 3, $b, "cdefg"})),
	 ?_assertEqual(true, part1:is_valid({policy, 2, 9, $c, "ccccccccc"}))].
