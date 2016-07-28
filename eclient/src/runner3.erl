-module(runner3).

-export([run/0]).

run() ->
	Self = self(),
	Count = 5, %000,
	ets:new(mytable2, [named_table, protected, set, {keypos, 1}]),
	ets:insert(mytable2, {ok, counter:new()}),
	ets:insert(mytable2, {err, counter:new()}),
	diameter:start(),
	Pids = [spawn_link(fun() -> Self ! {self(), {Y, one_client()}} end) || Y <- lists:seq(1, Count)],
	[ receive {Pid, R} -> R end || Pid <- Pids ],
	diameter:stop().

one_client()  ->
	client:start(),
	client:connect(tcp),
	timer:sleep(1000),
	X = 100, %000,
	recurse(X, {0, 0}),
	client:stop().


recurse(Count, {Ok, Er}) when Count == 0 ->
	io:format("Client Finished ~p, ~p \n", [Ok, Er]),
	ok;
	
recurse(Count, {Ok, Er}) ->
	timer:sleep((100)),
	{R, _} = client:call(),
	{AOk, AEr} = case R of 
		'ok' -> {Ok + 1, Er};
		_	-> {Ok, Er + 1}
	end,
	recurse(Count - 1, {AOk, AEr}).
