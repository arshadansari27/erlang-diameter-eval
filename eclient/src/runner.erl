-module(runner).

-export([run/0, one_client/0]).

run() ->
	ets:new(mytable2, [named_table, protected, set, {keypos, 1}]),
	ets:insert(mytable2, {ok, counter:new()}),
	ets:insert(mytable2, {err, counter:new()}),
	diameter:start(),
	client:start(),
	{X, Y} = client:connect(tcp),
	io:format("Connection ~p ~p\n", [X, Y]),
	one_client(),
	client:stop(),
	diameter:stop().


one_client()  ->
	X = 1,
	recurse(X, {0, 0}).


recurse(Count, {Ok, Er}) when Count == 0 ->
	io:format("Client Finished ~p, ~p \n", [Ok, Er]),
	ok;
	
recurse(Count, {Ok, Er}) ->
	timer:sleep((100)),
	{R, Wht} = client:call(),
	if 
		R == 'ok' ->
			{Ok2, Er2} = {Ok + 1, Er};
		true ->
			Error = lists:flatten(io_lib:format("Could not connect with reason: ~p", [Wht])),
			throw(Error),
			{Ok2, Er2} = {Ok, Er + 1}
	end,
	recurse(Count - 1, {Ok2, Er2}).
