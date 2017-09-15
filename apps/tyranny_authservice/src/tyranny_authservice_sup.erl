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
    Datasource = {authservice_datasource, {authservice_datasource, start, []}, permanent, 2000, worker, [authservice_datasource]},
    GameServiceHealth = {authservice_lb, {authservice_lb, start, []}, permanent, 2000, worker, [authservice_lb]},
    Children = [Datasource, GameServiceHealth],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
