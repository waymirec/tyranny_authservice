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
    application:start(ranch),
    {ok, _} = ranch:start_listener(tryanny_authservice,
				   ranch_tcp, 
				   [ {port, config:key(<<"listener.port">>)}, 
				     {num_acceptors, config:key(<<"listener.num_acceptors">>)}, 
				     {max_connections, config:key(<<"listener.max_connections">>)}
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
