-module(part1).
-export([start/0]).
-ifdef(TEST). -export([pairs/1]). -endif.

input() -> case io:fread("", "~d") of
		   {ok, [N]} -> [N] ++ input();
		   eof -> []
	   end.

pairs(List) -> [{A, B} || A <- List, B <- List].

start() ->
	InputPairs = pairs(input()),
	case lists:search(fun({A, B}) -> (A + B) =:= 2020 end, InputPairs) of
		{value, {A, B}} -> io:format("Result: ~p~n", [A * B]);
		false -> io:format("Value not found~n")
	end.
