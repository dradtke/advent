-module(day11_tests).
-include_lib("eunit/include/eunit.hrl").

small_input() -> [
	"L.LL.LL.LL",
	"LLLLLLL.LL",
	"L.L.L..L..",
	"LLLL.LL.LL",
	"L.LL.LL.LL",
	"L.LLLLL.LL",
	"..L.L.....",
	"LLLLLLLLLL",
	"L.LLLLLL.L",
	"L.LLLLL.LL"
				].

calculate_occupied_seats_part1_test_() ->
	{Map, Width, Height} = day11:parse(small_input()),
	[?_assertEqual(37, day11:count_occupied(part1:calculate_changes(Map, Width, Height)))].

calculate_occupied_seats_part2_test_() ->
	{Map, Width, Height} = day11:parse(small_input()),
	[?_assertEqual(26, day11:count_occupied(part2:calculate_changes(Map, Width, Height)))].
