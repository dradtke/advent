-module(day05_tests).
-include_lib("eunit/include/eunit.hrl").

lower_test_() ->
	[?_assertEqual({0, 63}, day05:lower({0, 127})),
	 ?_assertEqual({32, 47}, day05:lower({32, 63}))].

upper_test_() ->
	[?_assertEqual({64, 127}, day05:upper({0, 127})),
	 ?_assertEqual({48, 63}, day05:upper({32, 63}))].

locate_test_() ->
	[?_assertEqual(44, day05:locate("FBFBBFF", {0, 127})),
	 ?_assertEqual(5, day05:locate("RLR", {0, 7}))].

find_seat_test_() -> 
	[?_assertEqual({44, 5}, day05:find_seat("FBFBBFFRLR")),
	 ?_assertEqual({70, 7}, day05:find_seat("BFFFBBFRRR")),
	 ?_assertEqual({14, 7}, day05:find_seat("FFFBBBFRRR")),
	 ?_assertEqual({102, 4}, day05:find_seat("BBFFBBFRLL"))].
