-module(part1_tests).
-include_lib("eunit/include/eunit.hrl").

pairs_test() ->
	[?_assertEqual([{1,1},{1,2},{2,1},{2,2}], part1:pairs([1,2]))].
