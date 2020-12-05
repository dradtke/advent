-module(part1).
-export([start/0]).

start() ->
	Seats = lists:map(fun day05:find_seat/1, day05:input()),
	SeatIds = lists:map(fun day05:seat_id/1, Seats),
	io:format("Result: ~p~n", [lists:max(SeatIds)]).
