%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(relay_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_relay.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

peer_up(_SvcName, _Peer, State) ->
	[{_, Client_counter}] = ets:lookup(mytable_relay, client),
	other_counter:inc(Client_counter),
	io:format("Peer up ~p\n", [_Peer]),
    State.

peer_down(_SvcName, _Peer, State) ->
	[{_, Client_counter}] = ets:lookup(mytable_relay, client),
	other_counter:dec(Client_counter),
	io:format("Peer down ~p\n", [_Peer]),
    State.

%% Returning 'relay' from handle_request causes diameter to resend the
%% incoming request, which leads to pick_peer and prepare_request
%% callbacks as if sending explicitly. The 'extra' argument is
%% appended to the argument list for callbacks following from
%% resending of the request.

handle_request(_Pkt, _SvcName, _Peer) ->
	[{_, Counter}] = ets:lookup(mytable_relay, counter),
	counter:inc(Counter),
	[{_, Per_sec_counter}] = ets:lookup(mytable_relay, persec),
	other_counter:inc(Per_sec_counter),
    {relay, [{timeout, 1000}, {extra, [relayed]}]}.

%% diameter will filter the sender in the Peers list.
pick_peer(Peers, _, _SvcName, _State, relayed) ->
	[Peer | _] = Peers,
	{ok, Peer}.

%pick_peer(Peers, _, _SvcName, _State, relayed) ->
	%[{_, Peer_counter}] = ets:lookup(mytable_relay, peers),
	%Peer_count = other_counter:inc(Peer_counter),
	%Index = (Peer_count rem 2) + 1,
	% List = lists:map(fun(N) -> {Xs, _} = N, Xs end, Peers),
	%Peer = lists:nth(Index, Peers),
	%io:format("Picking ~p\n", [Peer]),
    %{ok, Peer}.

prepare_request(Pkt, _SvcName, _Peer, relayed) ->
	%#diameter_packet{header={Header}, avps=[Avps], errors=[]} = Pkt,
	%io:format("HEADER: ~p\n", [Header]),
	%io:format("AVPS: ~p\n", [Avps]),
	%Route = #diameter_avp{data={'Route-Record', "gxclient.seagullPCEF.org"}},
	%NPkt = #diameter_packet{header = {Header}, avps = [Avps | Route], errors = []},
	%io:format("PACKT: ~p\n", [NPkt]),
    %{send, NPkt}.
    {send, Pkt}.

prepare_retransmit(Pkt, _SvcName, _Peer, relayed) ->
    {send, Pkt}.

%% diameter expects handle_answer to return the diameter_packet record
%% containing the answer when called for a relayed request.

handle_answer(Pkt, _Request, _SvcName, _Peer, relayed) ->
	[{_, Ok_counter}] = ets:lookup(mytable_relay, ok),
	other_counter:inc(Ok_counter),
    Pkt.

handle_error(Reason, _Request, _SvcName, _Peer, relayed) ->
	[{_, Err_counter}] = ets:lookup(mytable_relay, err),
	other_counter:inc(Err_counter),
    {error, Reason}.
