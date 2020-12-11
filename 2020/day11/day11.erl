-module(day11).
-export([input/0, parse/1, calculate_changes/3, count_occupied/1]).

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

mapget(Map, X, Y) -> maps:get({X, Y}, Map, floor).
mapset(Map, X, Y, Value) -> maps:put({X, Y}, Value, Map).

adjacent_seats(Map, X, Y) ->
	Adjacents = [
				 mapget(Map, X-1, Y-1),
				 mapget(Map, X, Y-1),
				 mapget(Map, X+1, Y-1),
				 mapget(Map, X-1, Y),
				 mapget(Map, X+1, Y),
				 mapget(Map, X-1, Y+1),
				 mapget(Map, X, Y+1),
				 mapget(Map, X+1, Y+1)
				],
	lists:filter(fun is_seat/1, Adjacents).

is_occupied(Seat) -> Seat =:= occupied.
is_empty(Seat) -> Seat =:= empty.
is_seat(Seat) -> Seat =:= empty orelse Seat =:= occupied.

calculate_change(Map, X, Y) ->
	case mapget(Map, X, Y) of
		empty -> 
			case lists:all(fun is_empty/1, adjacent_seats(Map, X, Y)) of
				true -> occupied;
				false -> none
			end;
		occupied -> 
			NumOccupied = length(lists:filter(fun is_occupied/1, adjacent_seats(Map, X, Y))),
			if
				NumOccupied >= 4 -> empty;
				true -> none
			end;
		floor ->
			none
	end.

calculate_changes(Map, Width, Height) ->
	Seats = [{X, Y} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)],
	Actions = lists:map(fun({X, Y}) -> {X, Y, calculate_change(Map, X, Y)} end, Seats),
	Changes = lists:filter(fun({_, _, Action}) -> Action =/= none end, Actions),
	case Changes of
		[] -> Map;
		List ->
			NewMap = lists:foldl(fun({X, Y, Change}, Acc) -> mapset(Acc, X, Y, Change) end, Map, List),
			calculate_changes(NewMap, Width, Height)
	end.

count_occupied(Map) ->
	length(lists:filter(fun is_occupied/1, maps:values(Map))).
