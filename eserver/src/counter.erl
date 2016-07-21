-module(counter).

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
	receive 
		V -> 
			Curr_time = erlang:timestamp(),
			[{_, Start_time}] = ets:lookup(my_table, start_time),
			Tdiff = timer:now_diff(Curr_time, Start_time) / (1000 * 1000),
			{V, V / Tdiff}
	end.
