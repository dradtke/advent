-module(part2_tests).
-include_lib("eunit/include/eunit.hrl").

is_valid_test_() ->
	[?_assertEqual(true, part2:is_valid({policy, 1, 3, $a, "abcde"})),
	 ?_assertEqual(false, part2:is_valid({policy, 1, 3, $b, "cdefg"})),
	 ?_assertEqual(false, part2:is_valid({policy, 2, 9, $c, "ccccccccc"}))].
