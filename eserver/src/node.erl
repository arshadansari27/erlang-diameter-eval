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
         listen/3,
         connect/2,
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

-define(DEFAULT_PORT, 3869).

%% ---------------------------------------------------------------------------
%% Interface functions
%% ---------------------------------------------------------------------------

%% start/2

-spec start(diameter:service_name(), [diameter:service_opt()])
   -> ok
    | {error, term()}.

start(Name, Opts)
  when is_atom(Name), is_list(Opts) ->
  	io:format("Starting with options ~p\n", [Opts]),
    diameter:start_service(Name, Opts).

%% connect/2

-spec connect(diameter:service_name(), client_opts())
   -> {ok, diameter:transport_ref()}
    | {error, term()}.

connect(Name, Opts)
  when is_list(Opts) ->
    diameter:add_transport(Name, {connect, Opts});

connect(Name, {T, Opts}) ->
    connect(Name, Opts ++ client_opts(T));

connect(Name, T) ->
    connect(Name, [{connect_timer, 5000} | client_opts(T)]).

%% listen/2

-spec listen(diameter:service_name(), server_opts())
   -> {ok, diameter:transport_ref()}
    | {error, term()}.

listen(Name, Opts)
  when is_list(Opts) ->
    diameter:add_transport(Name, {listen, Opts});

listen(Name, {T, Opts}) ->
    listen(Name, Opts ++ server_opts(T));

listen(Name, T) ->
    listen(Name, server_opts(T)).

listen(Name, T, Port) ->
    listen(Name, server_opts({T, Port})).

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

server_opts({T, Addr, Port}) ->
  	io:format('Server opts ~p, ~p\n', [Addr, Port]),
    [{transport_module, tmod(T)},
     {transport_config, [{reuseaddr, true},
                         {ip, addr(Addr)},
                         {port, Port}]}];

server_opts({T, Port}) ->
  	io:format('Server opts ~p\n', [Port]),
    [{transport_module, tmod(T)},
     {transport_config, [{reuseaddr, true},
                         {ip, addr(loopback)},
                         {port, Port}]}];

server_opts(T) ->
  	io:format('Server opts ~p\n', [T]),
    server_opts({T, loopback, ?DEFAULT_PORT}).

%% client_opts/1
%%
%% Return transport options for a connecting transport.

client_opts({T, LA, RA, RP})
  when T == all;   %% backwards compatibility
       T == any ->
  	io:format('Client opts ~p\n', [T]),
    [[S, {C,Os}], T] = [client_opts({P, LA, RA, RP}) || P <- [sctp,tcp]],
    [S, {C,Os,2000} | T];

client_opts({T, LA, RA, RP}) ->
  	io:format('Client opts ~p\n', [T]),
    [{transport_module, tmod(T)},
     {transport_config, [{raddr, addr(RA)},
                         {rport, RP},
                         {reuseaddr, true}
                         | ip(LA)]}];

client_opts({T, RA, RP}) ->
  	io:format('Client opts ~p\n', [T]),
    client_opts({T, default, RA, RP});

client_opts(T) ->
  	io:format('Client opts ~p\n', [T]),
    client_opts({T, loopback, loopback, ?DEFAULT_PORT}).

%% ---------------------------------------------------------------------------

tmod(tcp)  -> diameter_tcp;
tmod(sctp) -> diameter_sctp.

ip(default) ->
    [];
ip(loopback) ->
    %[{ip, {127,0,0,1}}];
    [{ip, {172,16,101,168}}];

ip(Addr) ->
    [{ip, Addr}].

addr(loopback) ->
    % {127,0,0,1};
    {172,16,101,168};
    
addr(A) ->
    A.
