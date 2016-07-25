-module(server_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc6733.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

peer_up(_SvcName, _Peer, State) ->
	[{_, Client_counter}] = ets:lookup(my_table, client),
	Client_count = other_counter:inc(Client_counter),
	io:format("Client Connected, total clients ~p\n", [Client_count]),
    State.

peer_down(_SvcName, _Peer, State) ->
	[{_, Client_counter}] = ets:lookup(my_table, client),
	Client_count = other_counter:dec(Client_counter),
	io:format("Client disconnected, total clients ~p\n", [Client_count]),
    State.

pick_peer(_, _, _SvcName, _State) ->
    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
    ?UNEXPECTED.

%% A request whose decode was successful ...
handle_request(#diameter_packet{msg = Req, errors = []}, _SvcName, {_, Caps})
  when is_record(Req, diameter_base_RAR) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_RAR{'Session-Id' = Id,
                       'Re-Auth-Request-Type' = Type}
        = Req,
	[{_, Counter}] = ets:lookup(my_table, counter),
	counter:inc(Counter),
	[{_, Ok_counter}] = ets:lookup(my_table, ok),
	other_counter:inc(Ok_counter),
	[{_, Per_sec_counter}] = ets:lookup(my_table, persec),
	other_counter:inc(Per_sec_counter),
    {reply, #diameter_base_RAA{'Result-Code' = rc(Type),
                               'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'Session-Id' = Id}};

%% ... or one that wasn't. 3xxx errors are answered by diameter itself
%% but these are 5xxx errors for which we must contruct a reply.
%% diameter will set Result-Code and Failed-AVP's.
handle_request(#diameter_packet{msg = Req}, _SvcName, {_, Caps})
  when is_record(Req, diameter_base_RAR) ->
    #diameter_caps{origin_host = {OH,_},
                   origin_realm = {OR,_}}
        = Caps,
    #diameter_base_RAR{'Session-Id' = Id}
        = Req,
	[{_, Counter}] = ets:lookup(my_table, counter),
	counter:inc(Counter),
	[{_, Err_counter}] = ets:lookup(my_table, err),
	other_counter:inc(Err_counter),
	[{_, Per_sec_counter}] = ets:lookup(my_table, persec),
	other_counter:inc(Per_sec_counter),
    {reply, #diameter_base_RAA{'Origin-Host' = OH,
                               'Origin-Realm' = OR,
                               'Session-Id' = Id}};

%% Answer that any other message is unsupported.
handle_request(#diameter_packet{}, _SvcName, _) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED


%% Map Re-Auth-Request-Type to Result-Code just for the purpose of
%% generating different answers.

rc(0) ->
    2001;  %% DIAMETER_SUCCESS
rc(_) ->
    5012.  %% DIAMETER_UNABLE_TO_COMPLY
