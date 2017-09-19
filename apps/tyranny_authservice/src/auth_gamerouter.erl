-module(auth_gamerouter).
-behavior(gen_server).

-export([ init/1,
	  handle_cast/2,
	  handle_call/3,
	  handle_info/2,
	  terminate/2,
	  code_change/3
	]).

-export([ start/0,
	  stop/0,
	  get_servers/0
	]).

-include("authservice.hrl").

-define(HEALTH_TTL, 10000).

-record(server_info, {
        id = <<"">>				:: binary(),
	int_ip = <<"">>				:: binary(),
	ext_ip = <<"">>				:: binary(),
	ext_port = 0				:: integer(),
	num_conns = 0				:: integer(),
	sys_load_last = 100			:: integer(),
	sys_load_1min = 100			:: integer(),
	sys_load_5min = 100			:: integer(),
	sys_load_15min = 100			:: integer(),
	mem_total = 0				:: integer(),
	mem_free = 0				:: integer(),
	score = 0				:: integer(),
	pid					:: pid()
       }).

-type server_info() :: #server_info{}.

-record(state, {
	socket					:: term(), % socket()
	route_list = []				:: [game_server()],
	servers = #{}				:: map(),
	hello_timers = #{}			:: #{},
	route_update_timer			:: term() % tref()
       }).

-type state() :: #state{}.

-spec start() -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get_servers() -> Servers :: [server_info()].
get_servers() ->
    gen_server:call(?MODULE, get_servers).

-spec init(Args :: list()) -> {ok, State :: state()}.
init([]) ->
    Port = 12345,
    McastAddr = {224,0,0,100},
    {ok, Socket} = gen_udp:open(Port, [{reuseaddr,true}, {ip,McastAddr}, {multicast_ttl,4}, {multicast_loop,true}, binary]),
    inet:setopts(Socket,[{add_membership,{McastAddr,{0,0,0,0}}}, {active,once}]),
    {ok, TRef} = timer:send_interval(3000, self(), update_route_list),
    State = #state{socket=Socket, route_update_timer=TRef},
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(get_servers, _From, #state{route_list=[]}=State) ->
    {reply, undefined, State};

handle_call(get_servers, _From, State) ->
    #state{route_list=RouteList} = State,
    {reply, {ok, RouteList}, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(stop, State) ->
    #state{socket=Socket} = State,
    gen_udp:close(Socket),
    gen_server:cast(?MODULE, shutdown).

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({udp, Socket, RemoteIp, _LocalPort, <<ServerNameLength:8,
					   	   ServerName:ServerNameLength/binary,
					   	   ExternalIp:32,
					   	   ExternalPort:32,
					   	   NumConns:16,
					   	   SysLoadLast:8,
					   	   SysLoad1Min:8,
					   	   SysLoad5Min:8,
					   	   SysLoad15Min:8,
					   	   MemTotal:32,
					   	   MemFree:32>> = _Info}, #state{}=State) ->
    ServerInfo = #server_info{id=ServerName,
		     	      int_ip=RemoteIp,
		     	      ext_ip=inet_util:int_to_ip(ExternalIp),
		     	      ext_port=ExternalPort,
		     	      num_conns=NumConns,
		     	      sys_load_last=SysLoadLast,
		     	      sys_load_1min=SysLoad1Min,
		     	      sys_load_5min=SysLoad5Min,
		     	      sys_load_15min=SysLoad15Min,
		     	      mem_total=MemTotal,
		     	      mem_free=MemFree},

    {ok, NewState} = process_hello(ServerInfo, State),
    inet:setopts(Socket,[{active,once}]),
    {noreply, NewState};

handle_info({stats_expired, ServerName}, State) ->
    #state{servers=Servers,hello_timers=HelloTimers} = State,
    lager:debug("Hello expiration for server ~p", [ServerName]),
    UpdatedServers = maps:remove(ServerName, Servers),
    UpdatedHelloTimers = maps:remove(ServerName, HelloTimers),
    {noreply, State#state{servers=UpdatedServers, hello_timers=UpdatedHelloTimers}};
 
handle_info(update_route_list, State) ->
    #state{servers=Servers} = State,
    StatsList = lists:sort(fun(#server_info{score=Score1}, #server_info{score=Score2}) -> Score1 =< Score2 end, maps:values(Servers)),
    RouteList = lists:map(fun(#server_info{id=Id, int_ip=IntIp, ext_ip=ExtIp, ext_port=ExtPort}) -> #game_server{id=Id, int_ip=IntIp, ext_ip=ExtIp ,ext_port=ExtPort} end, StatsList),
    {noreply, State#state{route_list=RouteList}};

handle_info(Message, State) ->
    #state{socket=Socket} = State,
    lager:debug("Ignoring message: ~p", [Message]),
    inet:setopts(Socket,[{active,once}]),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec process_hello(ServerInfo :: server_info(), State :: state()) -> {ok, NewState :: state()}.
process_hello(#server_info{} = ServerInfo, #state{} = State) ->
    #state{servers=Servers} = State,
    #server_info{id=ServerName, ext_ip=ExtIp, ext_port=ExtPort} = ServerInfo,
    lager:debug("Got health stats for ~p/~p:~p", [ServerName, inet:ntoa(ExtIp), ExtPort]),

    Score = rand:uniform(10),
    UpdatedServerInfo = ServerInfo#server_info{score=Score},
    UpdatedServers = maps:put(ServerName, UpdatedServerInfo, Servers),

    {ok, State2} = cancel_hello_timer(ServerName, State),
    Timer = erlang:send_after(?HEALTH_TTL, self(), {stats_expired, ServerName}),
    HelloTimers = maps:put(ServerName, Timer, State2#state.hello_timers),
    {ok, State2#state{servers=UpdatedServers,hello_timers=HelloTimers}}.

-spec cancel_hello_timer(TimerId :: binary(), State :: state()) -> {ok, NewState :: state()}.
cancel_hello_timer(TimerId, State) ->
    #state{hello_timers=HelloTimers} = State,
    lager:debug("Canceling hello timer for server: ~p", [TimerId]),
    Timer = maps:get(TimerId, HelloTimers, badkey),
    cancel_hello_timer(TimerId, Timer, State).

cancel_hello_timer(TimerId, badkey, State) ->
    lager:debug("No timer found with id: ~p", [TimerId]),
    {ok, State};

cancel_hello_timer(TimerId, Timer, State) ->
    #state{hello_timers=HelloTimers} = State,
    lager:debug("Canceling hello timer: ~p", [TimerId]),
    erlang:cancel_timer(Timer),
    UpdatedHelloTimers = maps:remove(TimerId, HelloTimers),
    {ok, State#state{hello_timers=UpdatedHelloTimers}}.

