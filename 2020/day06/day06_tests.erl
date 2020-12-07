-module(day06_tests).
-include_lib("eunit/include/eunit.hrl").

parse_groups_any_test_() ->
	[?_assertEqual(sets:from_list([$a, $b, $c]), hd(day06:parse_groups_any(["abc"]))),
	 ?_assertEqual(sets:from_list([$a, $b, $c]), hd(day06:parse_groups_any(["a", "b", "c"]))),
	 ?_assertEqual(sets:from_list([$a, $b, $c]), hd(day06:parse_groups_any(["ab", "ac"]))),
	 ?_assertEqual(sets:from_list([$a]), hd(day06:parse_groups_any(["a", "a", "a", "a"]))),
	 ?_assertEqual([sets:from_list([$b]), sets:from_list([$a])], day06:parse_groups_any(["a", "", "b"]))].

parse_groups_all_test_() ->
	[?_assertEqual(sets:from_list([$a, $b, $c]), hd(day06:parse_groups_all(["abc"]))),
	 ?_assertEqual(sets:new(), hd(day06:parse_groups_all(["a", "b", "c"]))),
	 ?_assertEqual(sets:from_list([$a]), hd(day06:parse_groups_all(["ab", "ac"]))),
	 ?_assertEqual(sets:from_list([$a]), hd(day06:parse_groups_all(["a", "a", "a", "a"]))),
	 ?_assertEqual([sets:from_list([$b]), sets:from_list([$a])], day06:parse_groups_all(["a", "", "b"]))].
