%%%-------------------------------------------------------------------
%% @doc tyranny_authservice public API
%% @end
%%%-------------------------------------------------------------------

-module(tyranny_authservice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(tyranny_authservice, port),
    {ok, NumAcceptors} = application:get_env(tyranny_authservice, num_acceptors),
    {ok, MaxConns} = application:get_env(tyranny_authservice, max_connections),

    {ok, _} = ranch:start_listener(tryanny_authservice,
				   ranch_tcp, 
				   [{port, Port}, {num_acceptors, NumAcceptors}, {max_connections, MaxConns}], 
				   authservice_protocol, 
				   []),
    lager:start(),
    application:ensure_all_started (mongodb),
    tyranny_authservice_sup:start_link().


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
