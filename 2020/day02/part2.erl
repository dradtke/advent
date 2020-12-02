-module(part2).
-export([start/0, parse_line/1, is_valid/1]).

-record(policy, {pos1, pos2, char, password}).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [Line] ++ input()
		   end.

parse_line(Line) -> case io_lib:fread("~d-~d ~1ts: ~ts", Line) of
						{ok, [Pos1, Pos2, [Char], Password], _LeftOverChars} ->
							#policy{pos1=Pos1,
									pos2=Pos2,
									char=Char,
									password=Password};
						{more, _RestFormat, _Nchars, _InputStack} -> erlang:error("got more");
						{error, {fread, What}} -> erlang:error(What)
					end.

is_valid(#policy{pos1=Pos1, pos2=Pos2, char=Char, password=Password}) ->
	(lists:nth(Pos1, Password) =:= Char) xor (lists:nth(Pos2, Password) =:= Char).

start() ->
	Policies = lists:map(fun parse_line/1, input()),
	ValidPolicies = lists:filter(fun is_valid/1, Policies),
	io:format("Result: ~p~n", [length(ValidPolicies)]).
