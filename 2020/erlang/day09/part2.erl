-module(part2).
-export([start/0]).

start() ->
	N = 1212510616,
	Numbers = day09:parse(day09:input()),
	case day09:find_sum_set(Numbers, N) of
		undefined -> erlang:error(result_not_found);
		SumSet -> io:format("Result: ~p~n", [lists:min(SumSet) + lists:max(SumSet)])
	end.
