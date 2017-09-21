-module(gameservice_selection).
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
	list = []				:: [game_server()],
	update_timer				:: term()
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
    {ok, TRef} = timer:send_interval(3000, self(), update),
    State = #state{update_timer=TRef},
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
    Servers = gameservice_listener:servers(),
    List = build_game_servers(Servers),
    Sorted = lists:sort(fun(#game_server{score=Score1}, #game_server{score=Score2}) -> Score1 =< Score2 end, List),
    {noreply, State#state{list=Sorted}};

handle_info(Message, State) ->
    lager:debug("Ignoring message: ~p", [Message]),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


build_game_servers([#server_info{} = ServerInfo | T]) ->
    #server_info{id=Id, int_ip=IntIp, int_port=IntPort, ext_ip=ExtIp, ext_port=ExtPort} = ServerInfo,
    Score = calculate_score(ServerInfo),
    GameServer = #game_server{id=Id, int_ip=IntIp, int_port=IntPort, ext_ip=ExtIp, ext_port=ExtPort, score=Score},
    [GameServer | build_game_servers(T)];

build_game_servers([]) ->
    [].

calculate_score(#server_info{} = _ServerInfo) ->
    rand:uniform(10).
