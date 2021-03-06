-module(runner4).

-export([run/0]).

init() ->
	ets:new(mytable2, [named_table, protected, set, {keypos, 1}]),
	ets:insert(mytable2, {ok, counter:new()}),
	ets:insert(mytable2, {err, counter:new()}),
	dbg:tracer(),
	dbg:p(all, call),
	dbg:tp(diameter, add_transport, cx),
	dbg:tp(node, connect, cx),
	connect().

connect() ->
	diameter:start(),
	client:start(),
	Xval = client:connect(tcp),
	timer:sleep((1000)),
	io:format("Connect response: ~p\n", [Xval]).

stop() ->
	client:stop(),
	diameter:stop().

run() ->
	init(),
	timer:sleep((1000)),
	{R, Wht} = client:call(),	
	io:format("Response: ~p\n~p\n", [R, Wht]),
	timer:sleep((1000)),
	stop().


