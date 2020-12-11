-module(day10).
-export([input/0, adapters/1, adapter_set/1, adapt/2, count_adapt_all/2, count_adapt_all_improved/2, count_adapt_all_graph/1]).

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

count_adapt_all(_Input, []) -> 1;
count_adapt_all(Input, Outputs) ->
	Range = lists:seq(1, min(3, length(Outputs))),
	lists:foldl(fun(Next, Acc) ->
						Output = lists:nth(Next, Outputs),
						Diff = Output - Input,
						Acc + if
							Diff >= 1 andalso Diff =< 3 ->
									  count_adapt_all(Output, lists:sublist(Outputs, Next + 1, length(Outputs)));
							true -> 0
						end
				end, 0, Range).

adapter_set(Input) -> 
	List = lists:map(fun list_to_integer/1, Input),
	{sets:from_list(List), lists:max(List) + 3}.

count_adapt_all_improved(Target, AdapterSet) ->
	count_adapt_all_improved(1, Target, AdapterSet, 0)
	+ count_adapt_all_improved(2, Target, AdapterSet, 0)
	+ count_adapt_all_improved(3, Target, AdapterSet, 0).

count_adapt_all_improved(Input, Target, _AdapterSet, Acc) when Input =:= Target -> Acc + 1;

count_adapt_all_improved(Input, Target, AdapterSet, Acc) ->
	case sets:is_element(Input, AdapterSet) of
		false -> Acc;
		true -> count_adapt_all_improved(Input + 1, Target, AdapterSet, Acc)
				+ count_adapt_all_improved(Input + 2, Target, AdapterSet, Acc)
				+ count_adapt_all_improved(Input + 3, Target, AdapterSet, Acc)
	end.

build_graph(Adapters) ->
	Graph = digraph:new(),
	VertexList = [0|Adapters],
	VertexSet = sets:from_list(VertexList),
	lists:foreach(fun(N) -> digraph:add_vertex(Graph, N) end, VertexList),
	lists:foreach(fun(N) ->
						 try_add_edge(Graph, VertexSet, N, N+1),
						 try_add_edge(Graph, VertexSet, N, N+2),
						 try_add_edge(Graph, VertexSet, N, N+3)
				 end, VertexList),
	Graph.

try_add_edge(Graph, VertexSet, From, To) ->
	case sets:is_element(To, VertexSet) of
		true -> case digraph:add_edge(Graph, From, To) of
					{error, Desc} -> erlang:error({error, Desc});
					_ -> ok
				end;
		false -> noop
	end.

dfs(_G, V, T) when V =:= T -> 1;
dfs(G, V, T) ->
	lists:foldl(fun(V2, Acc) -> Acc + dfs(G, V2, T) end, 0, digraph:out_neighbours(G, V)).

count_adapt_all_graph(Adapters) ->
	dfs(build_graph(Adapters), 0, lists:max(Adapters)).
