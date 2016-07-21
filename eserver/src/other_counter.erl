-module(other_counter).

-export([new/0, inc/1, get/1, dec/1]).

new() ->
	spawn(fun() -> loop(0) end).


loop(N) ->
	receive
		{inc, From} ->
			From ! N + 1,
			loop(N + 1);
		{get, From} ->
			From ! N,
			loop(N);
		{dec, From} ->
			From ! N - 1,
			loop(N - 1)
	end.

inc(Pid) ->
	Pid ! {inc, self()},
	receive V -> V end.

dec(Pid) ->
	Pid ! {dec, self()},
	receive V -> V end.


get(Pid) ->
	Pid ! {get, self()},
	receive V -> V end.
