-module(part1_tests).
-include_lib("eunit/include/eunit.hrl").

example_batch() -> ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
		    "byr:1937 iyr:2017 cid:147 hgt:183cm",
		    "",
		    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
		    "hcl:#cfa07d byr:1929",
		    "",
		    "hcl:#ae17e1 iyr:2013",
		    "eyr:2024",
		    "ecl:brn pid:760753108 byr:1931",
		    "hgt:179cm",
		    "",
		    "hcl:#cfa07d eyr:2025 pid:166559648",
		    "iyr:2011 ecl:brn hgt:59in"].

parse_batch_test_() -> 
	[?_assertEqual(4, length(part1:parse_batch(example_batch())))].

parse_passport_test_() -> 
	Passport = ["ecl:gry pid:860033327 eyr:2020", "hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"],
	Parsed = part1:parse_passport(Passport),
	[?_assertEqual("gry", proplists:get_value(ecl, Parsed)),
	 ?_assertEqual("860033327", proplists:get_value(pid, Parsed)),
	 ?_assertEqual("2020", proplists:get_value(eyr, Parsed)),
	 ?_assertEqual("#fffffd", proplists:get_value(hcl, Parsed)),
	 ?_assertEqual("1937", proplists:get_value(byr, Parsed)),
	 ?_assertEqual("2017", proplists:get_value(iyr, Parsed))].

is_valid_test_() ->
	[?_assertEqual(true, part1:is_valid([{byr, x}, {iyr, x}, {eyr, x}, {hgt, x}, {hcl, x}, {ecl, x}, {pid, x}])),
	 ?_assertEqual(false, part1:is_valid([{byr, x}, {iyr, x}, {eyr, x}, {hgt, x}, {hcl, x}, {ecl, x}]))].
