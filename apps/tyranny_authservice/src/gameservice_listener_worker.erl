-module(gameservice_listener_worker).
-behavior(gen_server).

-export([ init/1,
	  handle_cast/2,
	  handle_call/3,
	  handle_info/2,
	  terminate/2,
	  code_change/3
	]).

-export([ start/1 ]).

-include("authservice.hrl").

-define(HEALTH_TTL, 10000).

-record(state, {
	parent,
        server_ip,
	server_port,
	info = #server_info{}			:: server_info(),
	hello_timer,
	game_client_pid = undefined
       }).

-type state() :: #state{}.

-spec start(Args :: list()) -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start([ServerIp, ServerPort]) ->
    Pid = proc_lib:spawn(?MODULE, init, [[self(), ServerIp, ServerPort]]),
    MonitorRef = monitor(process, Pid),
    {ok, {Pid, MonitorRef}}.
    %%{ok, proc_lib:spawn_opt(?MODULE, init, [[self(), ServerIp, ServerPort]], [monitor])}.

-spec init(Args :: list()) -> {ok, State :: state()}.
init([Parent, ServerIp, ServerPort]) ->
    %erlang:monitor(process, Parent),
    HelloTimer = erlang:send_after(?HEALTH_TTL, self(), expired),
    State = #state{parent=Parent, server_ip=ServerIp, server_port=ServerPort, hello_timer=HelloTimer},
    gen_server:enter_loop(?MODULE, [], State).

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(info, _From, State) ->
    #state{info=ServerInfo} = State,
    {reply, {info, ServerInfo}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast({hello, <<ServerNameLength:8,
		      ServerName:ServerNameLength/bitstring,
		      IntIp:32,
		      IntPort:32,
		      ExternalIp:32,
		      ExternalPort:32,
		      NumConns:16,
		      SysLoadLast:8,
		      SysLoad1Min:8,
		      SysLoad5Min:8,
		      SysLoad15Min:8,
		      MemTotal:32,
		      MemFree:32>> = _Info}, State) ->

    ServerInfo = #server_info{id=ServerName,
		     	      int_ip=inet_util:int_to_ip(IntIp),
			      int_port=IntPort,
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
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, none, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info(expired, State) ->
    #state{info=ServerInfo} = State,
    #server_info{id=Id} = ServerInfo,
    lager:debug("Hello expiration for server ~p", [Id]),
    {stop, expired, State};
 
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    lager:debug("Parent (~p) exited with reason (~p), shutting down...", [Pid, Reason]),
    {stop, {client_exited, Reason}, State};
    
handle_info(Message, State) ->
    lager:debug("Ignoring message: ~p", [Message]),
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    lager:debug("Worker (~p) terminated.", [self()]),
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec process_hello(ServerInfo :: server_info(), State :: state()) -> {ok, NewState :: state()}.
process_hello(#server_info{} = ServerInfo, #state{} = State) ->
    #state{hello_timer=HelloTimer, game_client_pid=ClientPid} = State,
    #server_info{id=ServerId, int_ip=IntIp, int_port=IntPort} = ServerInfo,
    lager:debug("Got health stats for ~p/~p:~p", [ServerId, inet:ntoa(IntIp), IntPort]),
    erlang:cancel_timer(HelloTimer),

    NewClientPid = case ClientPid of
        undefined ->
	    lager:debug("Opening connection to ~p/~p", [IntIp, IntPort]),
	    {ok, NewPid} = gameservice_client:start_link([IntIp, IntPort]),
	    NewPid;
	_ ->
	    ClientPid
    end,

    NewHelloTimer = erlang:send_after(?HEALTH_TTL, self(), expired),
    {ok, State#state{info=ServerInfo, hello_timer=NewHelloTimer, game_client_pid=NewClientPid}}.

