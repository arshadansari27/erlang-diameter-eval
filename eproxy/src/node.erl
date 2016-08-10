%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

%%
%% A library module used by the example Diameter nodes. Does little
%% more than provide an alternate/simplified transport configuration.
%%

-module(node).

-export([start/2,
         listen/2,
         connect/2,
         connect/3,
         stop/1]).

-type protocol()
   :: tcp | sctp.

-type ip_address()
   :: default
    | inet:ip_address().

-type server_transport()
   :: protocol()
    | {protocol(), ip_address(), non_neg_integer()}.

-type server_opts()
   :: server_transport()
    | {server_transport(), [diameter:transport_opt()]}
    | [diameter:transport_opt()].

-type client_transport()
   :: protocol() | any
    | {protocol() | any, ip_address(), non_neg_integer()}
    | {protocol() | any, ip_address(), ip_address(), non_neg_integer()}.

-type client_opts()
   :: client_transport()
    | {client_transport(), [diameter:transport_opt()]}
    | [diameter:transport_opt()].

%% The server_transport() and client_transport() config is just
%% convenience: arbitrary options can be specifed as a
%% [diameter:transport_opt()].

-define(RELAY_PORT, 3868).
-define(SERVER_PORT, 3868).
-define(SERVER_PORT1, 3871).
-define(SERVER_PORT2, 3872).
-define(REMOTE_IP1, {192,168,141,128}).
-define(REMOTE_IP2, {192,168,141,129}).
-define(LOCAL_IP, {0,0,0,0}).

%% ---------------------------------------------------------------------------
%% Interface functions
%% ---------------------------------------------------------------------------

%% start/2

-spec start(diameter:service_name(), [diameter:service_opt()])
   -> ok
    | {error, term()}.

start(Name, Opts)
  when is_atom(Name), is_list(Opts) ->
  	io:format("Starting...~p\n", [Opts]),
    diameter:start_service(Name, Opts).

%% connect/2

-spec connect(diameter:service_name(), client_opts())
   -> {ok, diameter:transport_ref()}
    | {error, term()}.

connect(Name, Opts)
  when is_list(Opts) ->
  	io:format("Connection Options ~p\n", [{connect, Opts}]),
    diameter:add_transport(Name, {connect, Opts});

connect(Name, {T, Opts}) ->
    connect(Name, Opts ++ client_opts(T));

connect(Name, T) ->
    connect(Name, [{connect_timer, 5000} | client_opts(T)]).

connect(Name, T, SName) ->
    connect(Name, [{connect_timer, 5000} | client_opts({T, SName ,true})]).

%% listen/2

-spec listen(diameter:service_name(), server_opts())
   -> {ok, diameter:transport_ref()}
    | {error, term()}.

listen(Name, Opts)
  when is_list(Opts) ->
  	io:format("Listening ~p\n", [Opts]),
    diameter:add_transport(Name, {listen, Opts});

listen(Name, {T, Opts}) ->
    listen(Name, Opts ++ server_opts(T));

listen(Name, T) ->
    listen(Name, server_opts(T)).

%% stop/1

-spec stop(diameter:service_name())
   -> ok
    | {error, term()}.

stop(Name) ->
    diameter:stop_service(Name).

%% ---------------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------------

%% server_opts/1
%%
%% Return transport options for a listening transport.

server_opts({T, _, Port}) ->
    [{transport_module, tmod(T)},
     {transport_config, [{reuseaddr, true},
                         {ip, addr(local)},
                         {port, Port}]}];

server_opts(T) ->
    server_opts({T, local, ?RELAY_PORT}).

%% client_opts/1
%%
%% Return transport options for a connecting transport.

client_opts({T, LA, RA, RP})
  when T == all;   %% backwards compatibility
       T == any ->
    [[S, {C,Os}], T] = [client_opts({P, LA, RA, RP}) || P <- [sctp,tcp]],
    [S, {C,Os,2000} | T];

client_opts({T, LA, RA, RP}) ->
    [{transport_module, tmod(T)},
     {transport_config, [{raddr, addr(RA)},
                         {rport, RP},
                         {reuseaddr, true}
                         | ip(LA)]}];

client_opts({T, SNum, Multi}) when Multi == true ->
	Snum = case SNum of 
		1 -> remote1;
		2 -> remote2
	end,
    client_opts({T, local, Snum, ?SERVER_PORT});


client_opts({T, RA, RP}) ->
    client_opts({T, default, RA, RP});

client_opts(T) ->
    client_opts({T, loopback, loopback, ?SERVER_PORT}).

%% ---------------------------------------------------------------------------

tmod(tcp)  -> diameter_tcp;
tmod(sctp) -> diameter_sctp.

ip(default) ->
    [];

ip(loopback) ->
    [{ip, {127,0,0,1}}];

ip(local) ->
    [{ip, ?LOCAL_IP}];

ip(remote1) ->
    [{ip, ?REMOTE_IP1}];

ip(remote2) ->
    [{ip, ?REMOTE_IP2}];

ip(Addr) ->
    [{ip, Addr}].

addr(local) ->
    ?LOCAL_IP;

addr(loopback) ->
    {127,0,0,1};

addr(remote1) ->
    ?REMOTE_IP1;

addr(remote2) ->
    ?REMOTE_IP2;

addr(A) ->
    A.
