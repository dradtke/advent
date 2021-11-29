-module(part1_tests).
-include_lib("eunit/include/eunit.hrl").

interpret_test_() ->
	[?_assertEqual([2,0,0,0,99], part1:show(part1:interpret([1,0,0,0,99]))),
	 ?_assertEqual([2,3,0,6,99], part1:show(part1:interpret([2,3,0,3,99]))),
	 ?_assertEqual([2,4,4,5,99,9801], part1:show(part1:interpret([2,4,4,5,99,0]))),
	 ?_assertEqual([30,1,1,4,2,5,6,0,99], part1:show(part1:interpret([1,1,1,4,99,5,6,0,99])))].
