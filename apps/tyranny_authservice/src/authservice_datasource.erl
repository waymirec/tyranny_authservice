-module(authservice_datasource).
-behavior(gen_server).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-export([
	 start/0,
	 stop/0,
	 get_account_by_username/1
	]).

-record(state, { 
	  	connection
	       }).  

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, shudown).

get_account_by_username(Username) when is_binary(Username) ->
	gen_server:call(?MODULE, {get_account_by_username, Username}).

init([]) ->
    {ok, Connection} = mc_worker_api:connect([{database, <<"tyranny_account">>}]),
    State = #state{connection=Connection},
    {ok, State}.

handle_call({get_account_by_username, Username}, _From, #state{connection=Connection} = State) ->
    Result = mc_worker_api:find_one(Connection, <<"tyranny_account">>, #{<<"username">> => Username}, #{}), 
    {reply, Result, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, Server, _Extra) ->
    {ok, Server}.

