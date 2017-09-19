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
    Datasource = {auth_datasource, {auth_datasource, start, []}, permanent, 2000, worker, [auth_datasource]},
    GameRouter = {auth_gamerouter, {auth_gamerouter, start, []}, permanent, 2000, worker, [auth_gamerouter]},
    Children = [Datasource, GameRouter],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
