-module(part2).
-export([start/0]).

start() ->
	Seats = lists:map(fun day05:find_seat/1, day05:input()),
	SeatIds = lists:map(fun day05:seat_id/1, Seats),
	PossibleSeats = lists:seq(lists:min(SeatIds), lists:max(SeatIds)),
	io:format("Result: ~p~n", [hd(PossibleSeats -- SeatIds)]).
