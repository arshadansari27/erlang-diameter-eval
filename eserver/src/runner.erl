-module(runner).

-export([run/1, run71/0, run72/0, runn/0]).

run71() ->
	run(3871).

run72() ->
	run(3872).

runn() ->
	run(3868).

run(Port) ->
	ets:new(my_table, [named_table, protected, set, {keypos, 1}]),
	ets:insert(my_table, {counter, counter:new()}),
	ets:insert(my_table, {persec, other_counter:new()}),
	ets:insert(my_table, {ok, other_counter:new()}),
	ets:insert(my_table, {err, other_counter:new()}),
	ets:insert(my_table, {client, other_counter:new()}),
	ets:insert(my_table, {start_time, erlang:timestamp()}),
	diameter:start(),
	Oh = lists:flatten(io_lib:format("~p.server.com", [Port])),
	Or =  "server.com", % lists:flatten(io_lib:format("~p.example.com", [Port])), % "example.com", 
	Pn = lists:flatten(io_lib:format("~p-Server-Erlang", [Port])),
	Vid = 193, %lists:flatten(io_lib:format("~p", [Port])),
	%{Sequence_start, Sequence_end} = case Port of 3871 -> {0, 15}; 3872 -> {16, 32} end,
	server:start([{'Origin-Host', Oh}, 
				  {'Origin-Realm', Or}, 
				  {'Product-Name', Pn}, 
				  {'Vendor-Id', Vid} %,
				  %{sequence, {Sequence_start, Sequence_end}}
				 ]),
	server:listen(tcp, Port, custom),
	Fname = lists:flatten(io_lib:format("output-~p.log", [Port])),
	{ok, LogFile} = file:open(Fname, write),
	spawn(fun() -> print_counts(0, LogFile) end),
	spawn(fun() -> print_process_count() end).


print_counts(N, LogFile) ->
	timer:sleep((1000)),
	[{_, Counter}] = ets:lookup(my_table, counter),
	[{_, Ok_counter}] = ets:lookup(my_table, ok),
	[{_, Err_counter}] = ets:lookup(my_table, err),
	[{_, Client_counter}] = ets:lookup(my_table, client),
	[{_, Per_sec_counter}] = ets:lookup(my_table, persec),
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
