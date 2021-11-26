-module(day11).
-export([input/0, parse/1, mapget/3, mapset/4, count_occupied/1, is_seat/1, is_empty/1, is_occupied/1]).

input() -> case io:get_line("") of
                           eof -> [];
                           {error, Desc} -> erlang:error(Desc);
                           Line -> [string:chomp(Line)] ++ input()
                   end.

parse(Input) ->
	Height = length(Input),
	Width = length(hd(Input)),
	Map = lists:foldl(fun({Y, Row}, RowAcc) ->
						lists:foldl(fun({X, Value}, Acc) ->
											maps:put({X, Y}, case Value of
																 $L -> empty;
																 $# -> occupied;
																 $. -> floor;
																 _ -> erlang:error(unexpected_value)
															 end, Acc)
									end, RowAcc, lists:zip(lists:seq(1, Width), Row))
				end, maps:new(), lists:zip(lists:seq(1, Height), Input)),
	{Map, Width, Height}.

mapget(Map, X, Y) -> case maps:is_key({X, Y}, Map) of
						 true -> maps:get({X, Y}, Map);
						 false -> oob
					 end.
mapset(Map, X, Y, Value) -> maps:put({X, Y}, Value, Map).

is_occupied(Seat) -> Seat =:= occupied.
is_empty(Seat) -> Seat =:= empty.
is_seat(Seat) -> Seat =:= empty orelse Seat =:= occupied.

count_occupied(Map) ->
	length(lists:filter(fun is_occupied/1, maps:values(Map))).
