-module(runner2).

-export([run/0]).

run() ->
	Self = self(),
	Count = 5000, %000,
	ets:new(mytable2, [named_table, protected, set, {keypos, 1}]),
	ets:insert(mytable2, {ok, counter:new()}),
	ets:insert(mytable2, {err, counter:new()}),
	diameter:start(),
	client:start(),
	Uv = client:connect(tcp),
	io:format("~p \n", [Uv]),
	Print_pid = spawn(fun() -> print_counts(0) end),
	register(print_process, Print_pid),
	timer:sleep(1000),
	Pids = [spawn_link(fun() -> Self ! {self(), {Y, one_client()}} end) || Y <- lists:seq(1, Count)],
	[ receive {Pid, R} -> R end || Pid <- Pids ],
	exit(whereis(print_process), ok),
	client:stop(),
	diameter:stop().

one_client()  ->
	X = 1000, %000,
	recurse(X, {0, 0}).


recurse(Count, {Ok, Er}) when Count == 0 ->
	io:format("Client Finished ~p, ~p \n", [Ok, Er]),
	ok;
	
recurse(Count, {Ok, Er}) ->
	timer:sleep((100)),
	{R, Reason} = client:call(),
	{AOk, AEr} = case R of 
		'ok' -> {Ok + 1, Er};
		_	-> io:format("~p\n", [Reason]), {Ok, Er + 1}
	end,
	recurse(Count - 1, {AOk, AEr}).

print_counts(N) ->
	timer:sleep((1000)),
	[{_, Ok_counter}] = ets:lookup(mytable2, ok),
	[{_, Err_counter}] = ets:lookup(mytable2, err),
	Ok = counter:get(Ok_counter),
	Err = counter:get(Err_counter),
	io:format("~p\t~p\t~p\n", [N, Ok, Err]),
	print_counts(N + 1).
