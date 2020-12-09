-module(day09).
-export([input/0, parse/1, parse/2, sum_pairs/1, find_no_sum/2, find_sum_set/2]).

input() -> case io:get_line("") of
                           eof -> [];
                           {error, Desc} -> erlang:error(Desc);
                           Line -> [string:chomp(Line)] ++ input()
                   end.

parse(Input) -> lists:map(fun list_to_integer/1, Input).
parse(Input, PreambleLength) -> lists:split(PreambleLength, parse(Input)).

sum_pairs(Numbers) -> [{A, B} || A <- Numbers, B <- Numbers, A =/= B].

find_no_sum(_Prev, []) -> undefined;
find_no_sum(Prev, [N|Tail]) ->
	case lists:any(fun({A, B}) -> (A + B) =:= N end, sum_pairs(Prev)) of
		false -> N;
		true -> find_no_sum(tl(Prev) ++ [N], Tail)
	end.

prefix_sum(Numbers, Length) -> lists:foldl(fun(Elem, Acc) -> Elem + Acc end, 0, lists:sublist(Numbers, Length)).

find_sum_set([], _N) -> undefined;
find_sum_set(Numbers, N) -> find_sum_set(Numbers, N, 1).
find_sum_set(Numbers, N, Length) ->
	Sum = prefix_sum(Numbers, Length),
	if
		Sum < N -> find_sum_set(Numbers, N, Length + 1);
		Sum > N -> find_sum_set(tl(Numbers), N);
		Sum =:= N -> lists:sublist(Numbers, Length)
	end.
