-module(counter).

-export([new/0, inc/1, get/1, dec/1, get_clear/1]).

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
		{getclear, From} ->
			From ! N,
			loop(0);
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

get_clear(Pid) ->
	Pid ! {getclear, self()},
	receive V -> V end.
