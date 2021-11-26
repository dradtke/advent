-module(day07_tests).
-include_lib("eunit/include/eunit.hrl").

-export([example_input/0]).

example_input() -> [
	"light red bags contain 1 bright white bag, 2 muted yellow bags.",
	"dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
	"bright white bags contain 1 shiny gold bag.",
	"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
	"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
	"dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
	"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
	"faded blue bags contain no other bags.",
	"dotted black bags contain no other bags."
		   ].

example_counting_input() -> [
	"shiny gold bags contain 2 dark red bags.",
	"dark red bags contain 2 dark orange bags.",
	"dark orange bags contain 2 dark yellow bags.",
	"dark yellow bags contain 2 dark green bags.",
	"dark green bags contain 2 dark blue bags.",
	"dark blue bags contain 2 dark violet bags.",
	"dark violet bags contain no other bags."
			    ].

parse_rule_part_test_() ->
	[?_assertEqual({1, "bright white"}, day07:parse_rule_part("1 bright white bag")),
	 ?_assertEqual({2, "muted yellow"}, day07:parse_rule_part("2 muted yellow bags"))].

parse_rule_test_() ->
	[?_assertEqual(maps:from_list([
				       {"light red", [{1, "bright white"}, {2, "muted yellow"}]}
				      ]), day07:parse_rule("light red bags contain 1 bright white bag, 2 muted yellow bags.")),
	 ?_assertEqual(maps:from_list([{"faded blue", []}]), day07:parse_rule("faded blue bags contain no other bags."))].

parse_rules_test_() ->
	[?_assertEqual(maps:from_list([
				       {"light red", [{1, "bright white"}, {2, "muted yellow"}]}
				      ]), day07:parse_rule("light red bags contain 1 bright white bag, 2 muted yellow bags."))].

can_carry_test_() ->
	AllRules = day07:parse_rules(example_input()),
	[?_assertEqual(true, day07:can_carry(AllRules, "muted yellow", "light red")),
	 ?_assertEqual(true, day07:can_carry(AllRules, "shiny gold", "light red")),
	 ?_assertEqual(false, day07:can_carry(AllRules, "shiny gold", "faded blue"))].

count_bags_test_() ->
	AllRules = day07:parse_rules(example_counting_input()),
	[?_assertEqual(126, day07:count_bags(AllRules, "shiny gold"))].
