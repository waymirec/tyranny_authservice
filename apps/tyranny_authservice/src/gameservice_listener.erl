-module(gameservice_listener).
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
	  servers/0
	]).

-include("authservice.hrl").

-define(HEALTH_TTL, 10000).

-record(state, {
	socket					:: term(),
	workers = #{}				:: map()
       }).

-type state() :: #state{}.

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec servers() -> Servers :: [server_info()].
servers() ->
    Servers = gen_server:call(?MODULE, servers),
    Servers.

-spec init(Args :: list()) -> {ok, State :: state()}.
init([]) ->
    Port = config:hello_port(),
    Addr = config:hello_addr(),
    {ok, Socket} = gen_udp:open(Port, [{reuseaddr,true}, {ip,Addr}, {multicast_ttl,4}, {multicast_loop,true}, binary]),
    inet:setopts(Socket,[{add_membership,{Addr,{0,0,0,0}}}, {active,once}]),
    State = #state{socket=Socket},
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(servers, _From, #state{workers=#{}}=State) ->
    {reply, [], State};

handle_call(servers, _From, State) ->
    #state{workers=Workers} = State,
    WorkerPids = [ Pid || Pid <- maps:values(Workers) ],
    Servers = lists:map(fun(Pid) -> gen_server:call(Pid, info) end, WorkerPids),
    {reply, Servers, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(stop, State) ->
    #state{socket=Socket} = State,
    gen_udp:close(Socket),
    {stop, requested, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info({udp, Socket, FromAddress, FromPort, Packet} = _Input, State) ->
    #state{workers=Workers} = State,
    {NewWorkers, WorkerPid} = case maps:is_key(FromAddress, Workers) of
        false ->
	    {ok, {Pid, _Ref}} = gameservice_listener_worker:start([FromAddress, FromPort]),
   	    {maps:put(FromAddress, Pid, Workers), Pid};
	true ->
	    {Workers, maps:get(FromAddress, Workers)}
    end,

    gen_server:cast(WorkerPid, {hello, Packet}), 
    State2 = State#state{workers=NewWorkers},
    inet:setopts(Socket,[{active,once}]),
    {noreply, State2};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    lager:debug("Got DOWN message for process ~p with reason ~p", [Pid, Reason]),
    #state{workers=Workers} = State,
    Pred = fun(_FromAddress, WorkerPid) -> WorkerPid =/= Pid end,
    NewWorkers = maps:filter(Pred, Workers),
    {noreply, State#state{workers=NewWorkers}};

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

