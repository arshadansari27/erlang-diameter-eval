-module(runner2).

-export([run/0]).

run() ->
	Self = self(),
	Count = 5000,
	ets:new(my_table2, [named_table, protected, set, {keypos, 1}]),
	ets:insert(my_table2, {counter2, counter:new()}),
	diameter:start(),
	client:start(),
	Uv = client:connect(tcp),
	io:format("~p \n", [Uv]),
	spawn(fun() -> print_counts(0) end),
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
	timer:sleep((100)),
	{R, _} = client:call(),
	if 
		R == 'ok' ->
			{Ok2, Er2} = {Ok + 1, Er};
		true ->
			{Ok2, Er2} = {Ok, Er + 1}
	end,
	recurse(Count - 1, {Ok2, Er2}).

print_counts(N) ->
	timer:sleep((1000)),
	[{_, Counter}] = ets:lookup(my_table2, counter2),
	Val = counter:get(Counter),
	io:format("~p\t~p\n", [N, Val]),
	print_counts(N + 1).
