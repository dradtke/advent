-module(part2_tests).
-include_lib("eunit/include/eunit.hrl").

fuel_test_() ->
	[?_assertEqual(2, part2:fuel(14)),
	 ?_assertEqual(966, part2:fuel(1969)),
	 ?_assertEqual(50346, part2:fuel(100756))].
