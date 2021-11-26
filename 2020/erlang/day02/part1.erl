-module(part1).
-export([start/0, parse_line/1, count_char/2, is_valid/1]).

-record(policy, {char_min, char_max, char, password}).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [Line] ++ input()
		   end.

parse_line(Line) -> case io_lib:fread("~d-~d ~1ts: ~ts", Line) of
						{ok, [CharMin, CharMax, [Char], Password], _LeftOverChars} ->
							#policy{char_min=CharMin,
									char_max=CharMax,
									char=Char,
									password=Password};
						{more, _RestFormat, _Nchars, _InputStack} -> erlang:error("got more");
						{error, {fread, What}} -> erlang:error(What)
					end.

count_char(Char, Password) -> length(lists:filter(fun(C) -> C =:= Char end, Password)).

is_valid(#policy{char_min=CharMin, char_max=CharMax, char=Char, password=Password}) ->
	Count = count_char(Char, Password),
	(Count >= CharMin) andalso (Count =< CharMax).

start() ->
	Policies = lists:map(fun parse_line/1, input()),
	ValidPolicies = lists:filter(fun is_valid/1, Policies),
	io:format("Result: ~p~n", [length(ValidPolicies)]).
