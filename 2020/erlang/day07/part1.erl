-module(part1).
-export([start/0]).

start() ->
	Target = "shiny gold",
	AllRules = day07:parse_rules(day07:input()),
	Options = maps:keys(maps:remove(Target, AllRules)),
	Result = lists:foldl(fun (Color, Acc) -> Acc + case day07:can_carry(AllRules, Target, Color) of
							       true -> 1;
							       _ -> 0
						       end
		   end, 0, Options),
	io:format("Result: ~p~n", [Result]).
