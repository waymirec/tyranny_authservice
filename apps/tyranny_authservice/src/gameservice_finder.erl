-module(gameservice_finder).
-behavior(gen_server).

-export([ init/1,
	  handle_cast/2,
	  handle_call/3,
	  handle_info/2,
	  terminate/2,
	  code_change/3
	]).

-export([ start_link/0,
	  stop/0,
	  list/0,
	  next/0
	]).

-include("authservice.hrl").

-define(HEALTH_TTL, 10000).

-record(state, {
	list = []				:: [server_info()],
	update_timer				:: term(),
	db
       }).

-type state() :: #state{}.

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec list() -> Servers :: [server_info()].
list() ->
    gen_server:call(?MODULE, list).

-spec next() -> Server :: server_info().
next() ->
    gen_server:call(?MODULE, next).

-spec init(Args :: list()) -> {ok, State :: state()}.
init([]) ->
    DbHost = config:key(<<"gameserver_db.host">>),
    DbPort = config:key(<<"gameserver_db.port">>),
    DbDatabase = config:key(<<"gameserver_db.database">>),
    DbPassword = config:key(<<"gameserver_db.password">>),
    ConnectTimeout = config:key(<<"gameserver_db.connect_timeout">>),
    ReconnectDelay = config:key(<<"gameserver_db.reconnect_delay">>),
    {ok, DbClient} = eredis:start_link(DbHost, DbPort, DbDatabase, DbPassword, ReconnectDelay, ConnectTimeout),
    {ok, TRef} = timer:send_interval(3000, self(), update),
    State = #state{update_timer=TRef, db=DbClient},
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(list, _From, #state{list=[]}=State) ->
    {reply, [], State};

handle_call(list, _From, State) ->
    #state{list=List} = State,
    {reply, List, State};

handle_call(next, _From, #state{list=[]}=State) ->
    {reply, undefined, State};

handle_call(next, _From, State) ->
    #state{list=List} = State,
    [H | _] = List,
    {reply, H, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(stop, State) ->
    {stop, requested, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info(update, State) ->
    #state{db=DbClient} = State,
    Servers = update(DbClient),
    {noreply, State#state{list=Servers}};

handle_info(Message, State) ->
    lager:debug("Ignoring message: ~p", [Message]),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update(DbClient) ->
    {ok, ServerKeys} = eredis:q(DbClient, ["KEYS", "*"]),
    ServerBins = get_server_data(DbClient, ServerKeys),
    Servers = build_server_info(ServerBins),
    Sorted = lists:sort(fun(#server_info{score=Score1}, #server_info{score=Score2}) -> Score1 =< Score2 end, Servers),
    Sorted.

get_server_data(DbClient, [Key | Rest]) ->
    {ok, Value} = eredis:q(DbClient, ["GET", Key]),
    [Value | get_server_data(DbClient, Rest)];

get_server_data(_DbClient, []) ->
    [].

build_server_info([ Bin | Rest ]) ->
    << NameLength:8,
       Name:NameLength/bitstring,
       Ip:32,
       Port:32,
       Score:16
    >> = Bin,
    
    Server = #server_info{id=Name, ip=inet_util:int_to_ip(Ip), port=Port, score=Score},
    [Server | build_server_info(Rest)];

build_server_info([]) ->
  [].
