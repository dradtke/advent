-module(part2).
-export([start/0, calculate_changes/3]).

look(Map, X, Y, Dx, Dy) ->
	NewX = X + Dx, NewY = Y + Dy,
	case day11:mapget(Map, NewX, NewY) of
		floor -> look(Map, NewX, NewY, Dx, Dy);
		oob -> none;
		Seat -> Seat
	end.

nearby_seats(Map, X, Y) ->
	Nearby = [
				 look(Map, X, Y, -1, -1),
				 look(Map, X, Y, 0, -1),
				 look(Map, X, Y, 1, -1),
				 look(Map, X, Y, -1, 0),
				 look(Map, X, Y, 1, 0),
				 look(Map, X, Y, -1, 1),
				 look(Map, X, Y, 0, 1),
				 look(Map, X, Y, 1, 1)
				],
	lists:filter(fun day11:is_seat/1, Nearby).

calculate_change(Map, X, Y) ->
	case day11:mapget(Map, X, Y) of
		empty -> 
			case lists:all(fun day11:is_empty/1, nearby_seats(Map, X, Y)) of
				true -> occupied;
				false -> none
			end;
		occupied -> 
			NumOccupied = length(lists:filter(fun day11:is_occupied/1, nearby_seats(Map, X, Y))),
			if
				NumOccupied >= 5 -> empty;
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
			NewMap = lists:foldl(fun({X, Y, Change}, Acc) -> day11:mapset(Acc, X, Y, Change) end, Map, List),
			calculate_changes(NewMap, Width, Height)
	end.

start() ->
	{Map, Width, Height} = day11:parse(day11:input()),
	Result = day11:count_occupied(calculate_changes(Map, Width, Height)),
	io:format("Result: ~p~n", [Result]).
