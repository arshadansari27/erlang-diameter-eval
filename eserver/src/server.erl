-module(server).

-export([start/1,    %% start a service
         start/2,    %%
         listen/2,   %% add a listening transport
         stop/1]).   %% stop a service

%% Convenience functions using the default service name.
-export([start/0,
         listen/1,
         stop/0]).

-define(DEF_SVC_NAME, ?MODULE).

%% The service configuration. In a server supporting multiple Diameter
%% applications each application may have its own, although they could all
%% be configured with a common callback module.
-define(SERVICE(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [0]},
                        {restrict_connections, false},
                        {string_decode, false},
                        {application, [{alias, common},
                                       {dictionary, diameter_gen_base_rfc6733},
                                       {module, server_cb}]}]).

%% start/1

start(Name)
  when is_atom(Name) ->
    start(Name, []);

start(Opts)
  when is_list(Opts) ->
    start(?DEF_SVC_NAME, Opts).

%% start/0

start() ->
    start(?DEF_SVC_NAME).

%% start/2

start(Name, Opts) ->
    node:start(Name, Opts ++ [T || {K,_} = T <- ?SERVICE(Name),
                                   false == lists:keymember(K, 1, Opts)]).

%% listen/2

listen(Name, T) ->
    node:listen(Name, T).

listen(T) ->
    listen(?DEF_SVC_NAME, T).

%% stop/1

stop(Name) ->
    node:stop(Name).

stop() ->
    stop(?DEF_SVC_NAME).
