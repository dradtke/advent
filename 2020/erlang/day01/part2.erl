-module(part2).
-export([start/0]).

input() -> case io:fread("", "~d") of
		   {ok, [N]} -> [N] ++ input();
		   eof -> []
	   end.

triples(List) -> [{A, B, C} || A <- List, B <- List, C <- List].

start() ->
	InputTriples = triples(input()),
	case lists:search(fun({A, B, C}) -> (A + B + C) =:= 2020 end, InputTriples) of
		{value, {A, B, C}} -> io:format("Result: ~p~n", [A * B * C]);
		false -> io:format("Value not found~n")
	end.
