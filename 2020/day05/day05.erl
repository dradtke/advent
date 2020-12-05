-module(day05).
-export([input/0, lower/1, upper/1, locate/2, find_seat/1, seat_id/1]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:trim(Line)] ++ input()
		   end.

lower({A, B}) -> {A, A + ((B-A) div 2)}.
upper({A, B}) -> {A + ((B-A) div 2) + 1, B}.

locate([], {A, _}) -> A;
locate([$F|Tail], Range) -> locate(Tail, lower(Range));
locate([$L|Tail], Range) -> locate(Tail, lower(Range));
locate([$B|Tail], Range) -> locate(Tail, upper(Range));
locate([$R|Tail], Range) -> locate(Tail, upper(Range)).

find_seat(Line) -> {
			locate(lists:sublist(Line, 1, 7), {0, 127}),
			locate(lists:sublist(Line, 8, 3), {0, 7})
		   }.

seat_id({Row, Column}) -> (Row * 8) + Column.
