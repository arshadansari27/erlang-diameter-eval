-module(client).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").
-include_lib("dict/rfc4006_cc.hrl").

-export([start/1,     %% start a service
         start/2,     %%
         connect/2,   %% add a connecting transport
         call/1,      %% send using the record encoding
         cast/1,      %% send using the list encoding and detached
         stop/1]).    %% stop a service

-export([start/0,
         connect/1,
         stop/0,
         call/0,
         cast/0]).

-define(DEF_SVC_NAME, ?MODULE).
-define(APP_ALIAS, common). % ?MODULE
-define(L, atom_to_list).
-define(DIAMETER_DICT_CCRA, diameter_gen_base_rfc6733  ). % rfc4006_cc
-define(DCCA_APPLICATION_ID, 0). % 4 4294967295 16777238
-define(ORIGIN_HOST, "gxclient2.seagullPCEF2.org").
-define(ORIGIN_REALM, "seagullPCEF2.org").
-define(CONTEXT_ID, "gprs@diameter.com").
%-define(DIAMETER_DICT_CCRA, diameter_gen_base_rfc4006_cc).
%% The service configuration. As in the server example, a client
%% supporting multiple Diameter applications may or may not want to
%% configure a common callback module on all applications.
-define(SERVICE(Name), [{'Origin-Host', "gxclient2.seagullPCEF2.org"},
                        {'Origin-Realm', "seagullPCEF2.org"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Client"},
                        {'Auth-Application-Id', [?DCCA_APPLICATION_ID]},
                        {string_decode, false},
                        {application, [{alias, ?APP_ALIAS}, 
                                       {dictionary, ?DIAMETER_DICT_CCRA},
                                       {module, client_cb}]}]).

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

%% connect/2

connect(Name, T) ->
    node:connect(Name, T).

connect(T) ->
    connect(?DEF_SVC_NAME, T).

%% call/1

call(Name) ->
    SId = diameter:session_id(?L(Name)),
  	%CCR = #rfc4006_cc_CCR{
        %'Session-Id' = SId,
        %'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
        %'Service-Context-Id' = "gprs@diameter.com",
        %'CC-Request-Type' = 1,
        %'CC-Request-Number' = 0,
        %'Subscription-Id' = [#'rfc4006_cc_Subscription-Id' {
                                %'Subscription-Id-Type' = "1",
                                %'Subscription-Id-Data' = "5511985231234" 
                            %}],
        %'Multiple-Services-Indicator' = [1]
    %},
	%io:format("Calling now..~p\n", [CCR]),
    %diameter:call(?DEF_SVC_NAME, ?APP_ALIAS, CCR, []).
	
    RAR = #diameter_base_RAR{'Session-Id' = SId,
                             'Auth-Application-Id' = 0,
                             'Re-Auth-Request-Type' = 0},
    diameter:call(Name, common, RAR, []).

call() ->
    call(?DEF_SVC_NAME).

%% cast/1

cast(Name) ->
    SId = diameter:session_id(?L(Name)),
    RAR = ['RAR', {'Session-Id', SId},
                  {'Auth-Application-Id', 0},
                  {'Re-Auth-Request-Type', 1}],
    diameter:call(Name, common, RAR, [detach]).
    %CCR = #rfc4006_cc_CCR{
        %'Session-Id' = SId,
        %'Auth-Application-Id' = ?DCCA_APPLICATION_ID,
        %'Service-Context-Id' = "gprs@diameter.com",
        %'CC-Request-Type' = 1,
        %'CC-Request-Number' = 0,
        %'Subscription-Id' = [#'rfc4006_cc_Subscription-Id' {
                                %'Subscription-Id-Type' = "1",
                                %'Subscription-Id-Data' = "5511985231234" 
                            %}],
        %'Multiple-Services-Indicator' = [1]
    %},
    %io:format("REQ2: ~p -> ~p\n", [SId, CCR]),
    %diameter:call(?DEF_SVC_NAME, ?APP_ALIAS, CCR, [detach]).

cast() ->
    cast(?DEF_SVC_NAME).

%% stop/1

stop(Name) ->
    node:stop(Name).

stop() ->
    stop(?DEF_SVC_NAME).
