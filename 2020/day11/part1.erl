-module(part1).
-export([start/0, calculate_changes/3]).

adjacent_seats(Map, X, Y) ->
	Adjacents = [
				 day11:mapget(Map, X-1, Y-1),
				 day11:mapget(Map, X, Y-1),
				 day11:mapget(Map, X+1, Y-1),
				 day11:mapget(Map, X-1, Y),
				 day11:mapget(Map, X+1, Y),
				 day11:mapget(Map, X-1, Y+1),
				 day11:mapget(Map, X, Y+1),
				 day11:mapget(Map, X+1, Y+1)
				],
	lists:filter(fun day11:is_seat/1, Adjacents).

calculate_change(Map, X, Y) ->
	case day11:mapget(Map, X, Y) of
		empty -> 
			case lists:all(fun day11:is_empty/1, adjacent_seats(Map, X, Y)) of
				true -> occupied;
				false -> none
			end;
		occupied -> 
			NumOccupied = length(lists:filter(fun day11:is_occupied/1, adjacent_seats(Map, X, Y))),
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
			NewMap = lists:foldl(fun({X, Y, Change}, Acc) -> day11:mapset(Acc, X, Y, Change) end, Map, List),
			calculate_changes(NewMap, Width, Height)
	end.

start() ->
	{Map, Width, Height} = day11:parse(day11:input()),
	Result = day11:count_occupied(calculate_changes(Map, Width, Height)),
	io:format("Result: ~p~n", [Result]).
