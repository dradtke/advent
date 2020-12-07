-module(part2).
-export([start/0]).

start() ->
	Target = "shiny gold",
	AllRules = day07:parse_rules(day07:input()),
	Result = day07:count_bags(AllRules, Target),
	io:format("Result: ~p~n", [Result]).
