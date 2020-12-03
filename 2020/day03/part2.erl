-module(part2).
-export([start/0]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:trim(Line)] ++ input()
		   end.

check_position({X, Y}, Map) ->
	MapWidth = length(lists:nth(Y + 1, Map)),
	if X >= MapWidth -> check_position({X - MapWidth, Y}, Map);
	   true -> case lists:nth(X + 1, lists:nth(Y + 1, Map)) of
				   $. -> open;
				   $# -> tree;
				   Square -> erlang:error({unexpected_map_square, {X, Y}, Square})
			   end
	end.

slide(Map, Slope) -> slide(Map, Slope, {0, 0}, 0).
slide(Map, {Dx, Dy}, {X, Y}, NumTrees) ->
	MapHeight = length(Map),
	if Y >= MapHeight -> NumTrees;
	   true -> slide(Map, {Dx, Dy}, {X + Dx, Y + Dy}, NumTrees + case check_position({X, Y}, Map) of
																	 tree -> 1;
																	 _ -> 0
																 end)
	end.

start() ->
	Map = input(),
	Slopes = [{1,1}, {3,1}, {5,1}, {7,1}, {1, 2}],
	TreeCounts = lists:map(fun (Slope) -> slide(Map, Slope) end, Slopes),
	Result = lists:foldl(fun (X, Prod) -> X * Prod end, 1, TreeCounts),
	io:format("Result: ~p~n", [Result]).
