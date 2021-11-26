-module(part1).
-export([start/0, parse_batch/1, parse_passport/1, is_valid/1]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:trim(Line)] ++ input()
		   end.

parse_batch(Batch) -> parse_batch(Batch, [], []).
parse_batch([], PassportAcc, BatchAcc) -> [parse_passport(PassportAcc)|BatchAcc];
parse_batch([Line|Tail], PassportAcc, BatchAcc) when length(Line) =:= 0 -> parse_batch(Tail, [], [parse_passport(PassportAcc)|BatchAcc]);
parse_batch([Line|Tail], PassportAcc, BatchAcc) -> parse_batch(Tail, [Line|PassportAcc], BatchAcc).

parse_passport([]) -> [];
parse_passport([Line|Tail]) ->
	Elems = string:lexemes(Line, " "),
	ParsedLine = lists:map(fun parse_passport_elem/1, Elems),
	lists:append(ParsedLine, parse_passport(Tail)).

parse_passport_elem(Elem) -> 
	[Key, Value] = string:split(Elem, ":"),
	{list_to_atom(Key), Value}.

is_valid(Passport) ->
	RequiredFields = [byr, iyr, eyr, hgt, hcl, ecl, pid],
	lists:all(fun (Field) -> proplists:is_defined(Field, Passport) end, RequiredFields).

start() ->
	Batch = parse_batch(input()),
	ValidPassports = lists:filter(fun is_valid/1, Batch),
	io:format("Result: ~p~n", [length(ValidPassports)]).
