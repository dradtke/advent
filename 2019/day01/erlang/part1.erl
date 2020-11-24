-module(part1).
-export([start/0, fuel/1]).

% Read numbers from stdin.
read() -> case io:fread("", "~d") of
			  {ok, [N]} -> [N] ++ read();
			  eof -> []
		  end.

% Unused, but demonstrates printing out a list of input.
show([]) -> io:format("");
show([Head | Tail]) -> io:format("~p~n", [Head]), show(Tail).

fuel(N) -> (N div 3) - 2.

run([]) -> 0;
run([Head | Tail]) -> fuel(Head) + run(Tail).

start() -> io:format("~p~n", [run(read())]).
