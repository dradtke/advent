-module(day10).
-export([input/0, adapters/1, adapt/2, count_adapt_all/2]).

input() -> case io:get_line("") of
                           eof -> [];
                           {error, Desc} -> erlang:error(Desc);
                           Line -> [string:chomp(Line)] ++ input()
                   end.

adapters(Input) ->
	Adapters = lists:sort(lists:map(fun list_to_integer/1, Input)),
	Adapters ++ [lists:max(Adapters) + 3].

adapt(Input, Outputs) -> adapt(Input, Outputs, maps:new()).

adapt(_Input, [], DiffCounts) -> DiffCounts;
adapt(Input, [Output|OutputTail], DiffCounts) ->
	Diff = Output - Input,
	if
		Diff =< 0 -> adapter_too_little;
		Diff >= 1 andalso Diff =< 3 ->
			Current = maps:get(Diff, DiffCounts, 0),
			adapt(Output, OutputTail, maps:put(Diff, Current + 1, DiffCounts));
		Diff > 3 -> adapter_too_big
	end.

count_adapt_all(Start, Adapters) ->
	erlang:erase(),
	do_count_adapt_all([Start|Adapters]).

do_count_adapt_all([]) -> 0;
do_count_adapt_all([_]) -> 1;
do_count_adapt_all([Head|Tail]) ->
	CanReach = lists:takewhile(fun (N) -> N - Head =< 3 end, Tail),
	CanReachEnumerated = lists:zip(lists:seq(1, length(CanReach)), CanReach),
	lists:sum(lists:map(fun({Index, N}) ->
								case erlang:get(N) of
									undefined ->
										V = do_count_adapt_all(lists:nthtail(Index-1, Tail)),
										erlang:put(N, V),
										V;
									V -> V
								end
						end, CanReachEnumerated)).
