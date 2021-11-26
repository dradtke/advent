-module(part2_tests).
-include_lib("eunit/include/eunit.hrl").

validate_birth_year_test_() -> 
	[?_assertEqual(true, part2:validate_birth_year("1920")),
	 ?_assertEqual(true, part2:validate_birth_year("2002")),
	 ?_assertEqual(false, part2:validate_birth_year("1919")),
	 ?_assertEqual(false, part2:validate_birth_year("2003")),
	 ?_assertEqual(false, part2:validate_birth_year("50")),
	 ?_assertEqual(false, part2:validate_birth_year("abc"))].

validate_issue_year_test_() -> 
	[?_assertEqual(true, part2:validate_issue_year("2010")),
	 ?_assertEqual(true, part2:validate_issue_year("2020")),
	 ?_assertEqual(false, part2:validate_issue_year("2009")),
	 ?_assertEqual(false, part2:validate_issue_year("2021")),
	 ?_assertEqual(false, part2:validate_issue_year("50")),
	 ?_assertEqual(false, part2:validate_issue_year("abc"))].

validate_expiration_year_test_() -> 
	[?_assertEqual(true, part2:validate_expiration_year("2020")),
	 ?_assertEqual(true, part2:validate_expiration_year("2030")),
	 ?_assertEqual(false, part2:validate_expiration_year("2019")),
	 ?_assertEqual(false, part2:validate_expiration_year("2031")),
	 ?_assertEqual(false, part2:validate_expiration_year("50")),
	 ?_assertEqual(false, part2:validate_expiration_year("abc"))].

validate_height_test_() ->
	[?_assertEqual(true, part2:validate_height("150cm")),
	 ?_assertEqual(true, part2:validate_height("193cm")),
	 ?_assertEqual(true, part2:validate_height("59in")),
	 ?_assertEqual(true, part2:validate_height("76in")),
	 ?_assertEqual(false, part2:validate_height("149cm")),
	 ?_assertEqual(false, part2:validate_height("194cm")),
	 ?_assertEqual(false, part2:validate_height("58in")),
	 ?_assertEqual(false, part2:validate_height("77in")),
	 ?_assertEqual(false, part2:validate_height("150")),
	 ?_assertEqual(false, part2:validate_height("ab"))].

validate_hair_color_test_() ->
	[?_assertEqual(true, part2:validate_hair_color("#123abc")),
	 ?_assertEqual(false, part2:validate_hair_color("#123abz")),
	 ?_assertEqual(false, part2:validate_hair_color("123abc"))].

validate_eye_color_test_() ->
	[?_assertEqual(true, part2:validate_eye_color("amb")),
	 ?_assertEqual(true, part2:validate_eye_color("blu")),
	 ?_assertEqual(true, part2:validate_eye_color("brn")),
	 ?_assertEqual(true, part2:validate_eye_color("gry")),
	 ?_assertEqual(true, part2:validate_eye_color("grn")),
	 ?_assertEqual(true, part2:validate_eye_color("hzl")),
	 ?_assertEqual(true, part2:validate_eye_color("oth")),
	 ?_assertEqual(false, part2:validate_eye_color("xxx"))].

validate_passport_id_test_() ->
	[?_assertEqual(true, part2:validate_passport_id("000000001")),
	 ?_assertEqual(false, part2:validate_passport_id("0123456789"))].
