-module(runner).

-export([run/0]).

run() ->
	ets:new(mytable_relay, [named_table, protected, set, {keypos, 1}]),
	ets:insert(mytable_relay, {counter, counter:new()}),
	ets:insert(mytable_relay, {persec, other_counter:new()}),
	ets:insert(mytable_relay, {ok, other_counter:new()}),
	ets:insert(mytable_relay, {err, other_counter:new()}),
	ets:insert(mytable_relay, {client, other_counter:new()}),
	ets:insert(mytable_relay, {peers, other_counter:new()}),
	ets:insert(mytable_relay, {start_time, erlang:timestamp()}),
	io:format("Staring Diameter..."),
	diameter:start(),
	io:format("Staring Relay..."),
	relay:start(),
	timer:sleep((1000)),
	io:format("Connecting Relay to Server1..."),
	relay:connect(tcp, 0, true),
	timer:sleep((1000)),
	%io:format("Connecting Relay to Server2..."),
	%relay:connect(tcp, 2, true),
	timer:sleep((1000)),
	io:format("Relay Listening for connections..."),
	relay:listen(tcp),
	timer:sleep((1000)),
	{ok, LogFile} = file:open('output.log', write),
	spawn(fun() -> print_counts(0, LogFile) end),
	spawn(fun() -> print_process_count() end).


print_counts(N, LogFile) ->
	timer:sleep((1000)),
	[{_, Counter}] = ets:lookup(mytable_relay, counter),
	[{_, Ok_counter}] = ets:lookup(mytable_relay, ok),
	[{_, Err_counter}] = ets:lookup(mytable_relay, err),
	[{_, Client_counter}] = ets:lookup(mytable_relay, client),
	[{_, Per_sec_counter}] = ets:lookup(mytable_relay, persec),
	{Count, Avg} = counter:get(Counter),
	Ok_count = other_counter:get(Ok_counter),
	Err_count = other_counter:get(Err_counter),
	Client_count = other_counter:get(Client_counter),
	Per_sec_count = other_counter:get_clear(Per_sec_counter),
	io:format(LogFile, "~p\t~p\t~p\t~p\t~p\t~p\t~p\n", [N, Client_count, Count, Avg, Ok_count, Err_count, Per_sec_count]),
	print_counts(N + 1, LogFile).


print_process_count() ->
	timer:sleep((5000)),
	io:format("C: [~p]\n", [length(erlang:processes())]),
	print_process_count().
