%%%-------------------------------------------------------------------
%% @doc tyranny_authservice public API
%% @end
%%%-------------------------------------------------------------------

-module(tyranny_authservice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(config(Key), application:get_env(tyranny_authservice, Key)).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    config:start_link(),
    
    application:start(ranch),
    {ok, _} = ranch:start_listener(tryanny_authservice,
				   ranch_tcp, 
				   [ {port, config:port()}, 
				     {num_acceptors, config:num_acceptors()}, 
				     {max_connections, config:max_connections()}
				   ], 
				   authservice_handler, 
				   []),

    application:ensure_all_started (mongodb),
    tyranny_authservice_sup:start_link().


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
