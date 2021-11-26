-module(day07).
-export([input/0, parse_rules/1, parse_rule/1, parse_rule_part/1, can_carry/3, count_bags/2]).

input() -> case io:get_line("") of
			   eof -> [];
			   {error, Desc} -> erlang:error(Desc);
			   Line -> [string:chomp(Line)] ++ input()
		   end.

parse_rules(Input) ->
	ParsedRules = lists:map(fun parse_rule/1, Input),
	lists:foldl(fun maps:merge/2, maps:new(), ParsedRules).

parse_rule(Line) ->
	[Color, Rule] = string:split(Line, " bags contain "),
	case Rule of
		"no other bags." -> maps:put(Color, [], maps:new());
		_ -> RuleParts = string:split(string:trim(Rule, trailing, "."), ", ", all),
		     maps:put(Color, lists:map(fun parse_rule_part/1, RuleParts), maps:new())
	end.

parse_rule_part(RulePart) ->
	[N|Tail] = string:lexemes(RulePart, " "),
	{list_to_integer(N), string:join(lists:droplast(Tail), " ")}.

can_carry(AllRules, Target, Current) ->
	case maps:get(Current, AllRules) of
		[] -> false;
		Rules -> lists:any(fun({_N, Color}) -> Color =:= Target orelse can_carry(AllRules, Target, Color) end, Rules)
	end.

count_bags(AllRules, Current) ->
	Rules = maps:get(Current, AllRules),
	lists:foldl(fun({N, Color}, Acc) -> Acc + N + (N * count_bags(AllRules, Color)) end, 0, Rules).
