-module(part1_tests).
-include_lib("eunit/include/eunit.hrl").

fuel_test_() ->
	[?_assertEqual(2, part1:fuel(12)),
	 ?_assertEqual(2, part1:fuel(14)),
	 ?_assertEqual(654, part1:fuel(1969)),
	 ?_assertEqual(33583, part1:fuel(100756))].
