-module(day06).
-export([input/0, parse_groups_any/1, parse_groups_all/1]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:trim(Line)] ++ input()
		   end.

parse_groups_any(Input) -> parse_groups_any(Input, sets:new(), []).

parse_groups_any([], AnswersAcc, GroupsAcc) ->
	[AnswersAcc|GroupsAcc];
parse_groups_any([Line|Tail], AnswersAcc, GroupsAcc) when length(Line) =:= 0 ->
	parse_groups_any(Tail, sets:new(), [AnswersAcc|GroupsAcc]);
parse_groups_any([Line|Tail], AnswersAcc, GroupsAcc) ->
	parse_groups_any(Tail, sets:union(AnswersAcc, sets:from_list(Line)), GroupsAcc).

parse_groups_all(Input) -> parse_groups_all(Input, [], []).

parse_groups_all([], AnswersAcc, GroupsAcc) ->
	[sets:intersection(AnswersAcc)|GroupsAcc];
parse_groups_all([Line|Tail], AnswersAcc, GroupsAcc) when length(Line) =:= 0 ->
	parse_groups_all(Tail, [], [sets:intersection(AnswersAcc)|GroupsAcc]);
parse_groups_all([Line|Tail], AnswersAcc, GroupsAcc) ->
	parse_groups_all(Tail, [sets:from_list(Line)|AnswersAcc], GroupsAcc).
