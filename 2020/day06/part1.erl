-module(part1).
-export([start/0]).

start() ->
	Groups = day06:parse_groups_any(day06:input()),
	Sum = lists:foldl(fun(Group, Sum) -> sets:size(Group) + Sum end, 0, Groups),
	io:format("Result: ~p~n", [Sum]).
