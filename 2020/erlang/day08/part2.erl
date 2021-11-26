-module(part2).
-export([start/0]).

start() ->
	OriginalProgram = day08:parse_program(day08:input()),
	ModifiedPrograms = day08:modified_program_options(OriginalProgram),
	Result = lists:foldl(fun(Program, PriorResult) ->
						{_, Acc, Success} = day08:run(Program),
						case Success of
							true -> Acc;
							_ -> PriorResult
						end
				end, 0, ModifiedPrograms),
	io:format("Result: ~p~n", [Result]).
