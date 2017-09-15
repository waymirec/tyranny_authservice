-module(authservice_lb).
-behavior(gen_server).

-export([
	 init/1,
	 handle_cast/2,
	 handle_call/3,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-export([
	 start/0,
	 stop/0,
         get_server/0
	]).

-define(HEALTH_TTL, 10000).

-record(health, {
	  	  name,
		  ip,
		  ext_ip,
		  ext_port,
		  num_conns,
		  sys_load_last,
		  sys_load_1min,
		  sys_load_5min,
		  sys_load_15min,
		  mem_total,
		  mem_free,
		  score
	        }).

-record(server, {
		 name,
		 ip,
		 port
		}).
-type server() :: #server{}.

-record(state, {
	  	socket				:: term(), % socket()
		servers = []			:: [server()],
		stats = #{}			:: #{},
		stat_timers = #{}		:: #{},
		route_timer			:: term() % tref()
	       }).
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

get_server() ->
    gen_server:call(?MODULE, get_server).

init([]) ->
    Port = 12345,
    McastAddr = {224,0,0,100},
    {ok, Socket} = gen_udp:open(Port, [{reuseaddr,true}, {ip,McastAddr}, {multicast_ttl,4}, {multicast_loop,true}, binary]),
    inet:setopts(Socket,[{add_membership,{McastAddr,{0,0,0,0}}}, {active,once}]),
    {ok, TRef} = timer:send_interval(3000, self(), update_servers),
    State = #state{socket=Socket, route_timer=TRef},
    {ok, State}.

handle_call(get_server, _From, #state{servers=[]}=State) ->
    {reply, undefined, State};

handle_call(get_server, _From, State) ->
    [Server | _] = State#state.servers,
    {reply, {ok, Server}, State}.    

handle_cast(stop, State) ->
    gen_udp:close(State#state.socket),
    gen_server:cast(?MODULE, shutdown).

handle_info({udp, Socket, RemoteIp, _LocalPort, <<NameLength:8,
					   	   Name:NameLength/binary,
					   	   ExternalIp:32,
					   	   ExternalPort:32,
					   	   NumConns:16,
					   	   SysLoadLast:8,
					   	   SysLoad1Min:8,
					   	   SysLoad5Min:8,
					   	   SysLoad15Min:8,
					   	   MemTotal:32,
					   	   MemFree:32>> = _Info}, #state{}=State) ->
    Health = #health{name=Name,
		     ip=RemoteIp,
		     ext_ip=inet_util:int_to_ip(ExternalIp),
		     ext_port=ExternalPort,
		     num_conns=NumConns,
		     sys_load_last=SysLoadLast,
		     sys_load_1min=SysLoad1Min,
		     sys_load_5min=SysLoad5Min,
		     sys_load_15min=SysLoad15Min,
		     mem_total=MemTotal,
		     mem_free=MemFree},

    {ok, NewState} = process_stats(Health, State),
    inet:setopts(Socket,[{active,once}]),
    {noreply, NewState};

handle_info({stats_expired, ServerName}, State) ->
    lager:debug("Stats expired for server ~p", [ServerName]),
    Stats = maps:remove(ServerName, State#state.stats),
    Timers = maps:remove(ServerName, State#state.stat_timers),
    {noreply, State#state{stats=Stats, stat_timers=Timers}};
 
handle_info(update_servers, State) ->
    Stats = lists:sort(fun(H1, H2) -> H1#health.score =< H2#health.score end, maps:values(State#state.stats)),
    Servers = lists:map(fun(#health{name=Name,ext_ip=Ip,ext_port=Port}) -> #server{name=Name,ip=Ip,port=Port} end, Stats),
    {noreply, State#state{servers=Servers}};

handle_info(Message, State) ->
    lager:debug("Ignoring message: ~p", [Message]),
    inet:setopts(State#state.socket,[{active,once}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_stats(#health{name=ServerName} = Health, State) ->
    lager:debug("Got health stats for ~p/~p:~p", [Health#health.name, inet:ntoa(Health#health.ext_ip), Health#health.ext_port]),

    Score = rand:uniform(10),
    Health2 = Health#health{score=Score},
    Stats = maps:put(ServerName, Health2, State#state.stats),

    {ok, State2} = cancel_stat_timer(ServerName, State),
    Timer = erlang:send_after(?HEALTH_TTL, self(), {stats_expired, Health#health.name}),
    Timers = maps:put(ServerName, Timer, State2#state.stat_timers),
    {ok, State2#state{stats=Stats,stat_timers=Timers}}.

cancel_stat_timer(TimerId, State) ->
    lager:debug("Canceling stats timer for server: ~p", [TimerId]),
    Timer = maps:get(TimerId, State#state.stat_timers, badkey),
    cancel_stat_timer(TimerId, Timer, State).

cancel_stat_timer(TimerId, badkey, State) ->
    lager:debug("No timer found with id: ~p", [TimerId]),
    {ok, State};

cancel_stat_timer(TimerId, Timer, State) ->
    lager:debug("Canceling stats timer: ~p", [TimerId]),
    erlang:cancel_timer(Timer),
    NewTimers = maps:remove(TimerId, State#state.stat_timers),
    {ok, State#state{stat_timers=NewTimers}}.

