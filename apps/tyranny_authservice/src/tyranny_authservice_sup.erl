%%%-------------------------------------------------------------------
%% @doc tyranny_authservice top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tyranny_authservice_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Datasource = {account_datasource, {account_datasource, start, []}, permanent, 2000, worker, [account_datasource]},
    HelloServer = {gameservice_listener, {gameservice_listener, start_link, []}, permanent, 2000, worker, [gameservice_listener]},
    GameRouter = {gameservice_selection, {gameservice_selection, start_link, []}, permanent, 2000, worker, [gameservice_selection]},
    Children = [Datasource, HelloServer, GameRouter],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
