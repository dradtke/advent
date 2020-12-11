-module(day10_tests).
-include_lib("eunit/include/eunit.hrl").

small_input() -> ["16", "10", "15", "5", "1", "11", "7", "19", "6", "12", "4"].
small_adapters() -> day10:adapters(small_input()).

large_input() -> ["28", "33", "18", "42", "31", "14", "46", "20", "48", "47", "24", "23", "49", "45", "19", "38", "39", "11", "1", "32", "25", "35", "8", "17", "7", "9", "4", "2", "34", "10", "3"].
large_adapters() -> day10:adapters(large_input()).

adapt_test_() ->
	[?_assertEqual(maps:from_list([{1, 7}, {3, 5}]), day10:adapt(0, small_adapters())),
	 ?_assertEqual(maps:from_list([{1, 22}, {3, 10}]), day10:adapt(0, large_adapters()))].

count_adapt_all_test_() ->
	{SmallAdapterSet, SmallTarget} = day10:adapter_set(small_input()),
	{LargeAdapterSet, LargeTarget} = day10:adapter_set(large_input()),
	[?_assertEqual(8, day10:count_adapt_all_improved(SmallTarget, SmallAdapterSet)),
	 ?_assertEqual(19208, day10:count_adapt_all_improved(LargeTarget, LargeAdapterSet))].

count_adapt_all_graph_test_() ->
	[?_assertEqual(8, day10:count_adapt_all_graph(small_adapters())),
	 ?_assertEqual(19208, day10:count_adapt_all_graph(large_adapters()))].
