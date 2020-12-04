-module(part2).
-export([
	 start/0,
	 validate_birth_year/1,
	 validate_issue_year/1,
	 validate_expiration_year/1,
	 validate_height/1,
	 validate_hair_color/1,
	 validate_eye_color/1,
	 validate_passport_id/1
	]).

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
	Elems = string:split(Line, " ", all),
	ParsedLine = lists:map(fun parse_passport_elem/1, Elems),
	lists:append(ParsedLine, parse_passport(Tail)).

parse_passport_elem(Elem) -> 
	[Key, Value] = string:split(Elem, ":"),
	{list_to_atom(Key), Value}.

is_valid(Passport) ->
	validate_birth_year(proplists:get_value(byr, Passport)) andalso
	validate_issue_year(proplists:get_value(iyr, Passport)) andalso
	validate_expiration_year(proplists:get_value(eyr, Passport)) andalso
	validate_height(proplists:get_value(hgt, Passport)) andalso
	validate_hair_color(proplists:get_value(hcl, Passport)) andalso
	validate_eye_color(proplists:get_value(ecl, Passport)) andalso
	validate_passport_id(proplists:get_value(pid, Passport)).

validate_birth_year(undefined) -> false;
validate_birth_year(Value) -> case io_lib:fread("~4d", Value) of
				    {ok, [N], []} -> (N >= 1920) andalso (N =< 2002);
				    _ -> false
			    end.

validate_issue_year(undefined) -> false;
validate_issue_year(Value) -> case io_lib:fread("~4d", Value) of
				    {ok, [N], []} -> (N >= 2010) andalso (N =< 2020);
				    _ -> false
			    end.

validate_expiration_year(undefined) -> false;
validate_expiration_year(Value) -> case io_lib:fread("~4d", Value) of
				    {ok, [N], []} -> (N >= 2020) andalso (N =< 2030);
				    _ -> false
			    end.

validate_height(undefined) -> false;
validate_height(Value) -> 
	RawHeight = lists:sublist(Value, 1, length(Value)-2),
	case io_lib:fread("~d", RawHeight) of
		{ok, [Height], []} -> 
			Units = lists:sublist(Value, length(Value)-1, 2),
			case Units of
				"cm" -> (Height >= 150) andalso (Height =< 193);
				"in" -> (Height >= 59) andalso (Height =< 76);
				_ -> false
			end;
		_ -> false
	end.

validate_hair_color(undefined) -> false;
validate_hair_color([$#|Hex]) ->
	{ok, Regex} = re:compile("^[0-9a-f]{6}$"),
	case re:run(Hex, Regex) of
		{match, _} -> true;
		_ -> false
	end;
validate_hair_color(_) -> false.

validate_eye_color(undefined) -> false;
validate_eye_color("amb") -> true;
validate_eye_color("blu") -> true;
validate_eye_color("brn") -> true;
validate_eye_color("gry") -> true;
validate_eye_color("grn") -> true;
validate_eye_color("hzl") -> true;
validate_eye_color("oth") -> true;
validate_eye_color(_) -> false.

validate_passport_id(undefined) -> false;
validate_passport_id(Value) ->
	{ok, Regex} = re:compile("^[0-9]{9}$"),
	case re:run(Value, Regex) of
		{match, _} -> true;
		_ -> false
	end.

start() ->
	Batch = parse_batch(input()),
	ValidPassports = lists:filter(fun is_valid/1, Batch),
	io:format("Result: ~p~n", [length(ValidPassports)]).
