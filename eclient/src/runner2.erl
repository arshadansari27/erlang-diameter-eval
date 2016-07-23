-module(runner2).

-export([run/0]).

run() ->
	Self = self(),
	Count = 1000,
	diameter:start(),
	client:start(),
	Uv = client:connect(tcp),
	io:format("~p \n", [Uv]),
	timer:sleep(1000),
	Pids = [spawn_link(fun() -> Self ! {self(), {Y, one_client()}} end) || Y <- lists:seq(1, Count)],
	[ receive {Pid, R} -> R end || Pid <- Pids ],
	client:stop(),
	diameter:stop().

one_client()  ->
	X = 10000,
	recurse(X, {0, 0}).


recurse(Count, {Ok, Er}) when Count == 0 ->
	io:format("Client Finished ~p, ~p \n", [Ok, Er]),
	ok;
	
recurse(Count, {Ok, Er}) ->
	timer:sleep(1),
	{R, _} = client:call(),
	if 
		R == 'ok' ->
			{Ok2, Er2} = {Ok + 1, Er};
		true ->
			{Ok2, Er2} = {Ok, Er + 1}
	end,
	recurse(Count - 1, {Ok2, Er2}).
