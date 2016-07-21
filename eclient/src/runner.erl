-module(runner).

-export([run/0, stop_client/1]).

run() ->
	one_client().

print_pids([Pid|Rest]) ->
	io:format("~p ", [Pid]),
	print_pids(Rest);

print_pids([]) ->
	io:format("\n"),
	ok.

one_client()  ->
	Tstart = erlang:timestamp(),
	diameter:start(),
	client:start(),
	Uv = client:connect(tcp),
	io:format("~p \n", [Uv]),
	timer:sleep(1000),
	Self = self(),
	Pids = [spawn_link(fun() -> Self ! {self(), {Y, run_client()}} end) || Y <- lists:seq(1, 10)],
	print_pids(Pids),
	spawn(fun() -> my_stop(Pids, Tstart) end).

my_stop(Pids, Tstart) ->
	receive 
		{stop, From} ->
			try [ exit(P_id, 0) || P_id <- Pids ] of 
				_ -> ok
			catch
				_ -> io:format("Error Happend")
			end,
			client:stop(),
			diameter:stop(),
			Tend = erlang:timestamp(),
			Time = timer:now_diff(Tend, Tstart),
			io:format("Total time tuple indicates: ~p seconds.~n", [Time / (1000 * 1000)]),
			From ! done,
			exit(0)
	end.

stop_client(Pid) ->
	Pid ! {stop, self()},
	receive V -> V end.	
	
run_client() ->
	recurse({0, 0}).
	
recurse({Ok, Er}) ->
	{R, _} = client:call(),
	if 
		R == 'ok' ->
			{Ok2, Er2} = {Ok + 1, Er};
		true ->
			{Ok2, Er2} = {Ok, Er + 1}
	end,
	recurse({Ok2, Er2}).
