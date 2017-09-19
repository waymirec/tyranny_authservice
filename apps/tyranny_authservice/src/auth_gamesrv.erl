-module(auth_gamesrv).
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
         start/1,
         stop/0
        ]).

-record(state, {
	parent_pid				:: pid(),
	server_host = <<"">>			:: binary(),
	server_port = 0				:: integer(),
	socket					:: gen_tcp:socket()
       }).

-type state() :: #state{}.

-define(SOCKET_TIMEOUT, 5000).

-spec start(list()) -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start([Host, Port, Parent]) when is_binary(Host), is_integer(Port), is_pid(Parent) ->
    gen_server:start_link(?MODULE, [Host, Port, Parent], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec init(Args :: list()) -> {ok, State :: state(), Timeout :: integer() | infinity}.
init([Host, Port, Parent]) ->
    {ok, Socket} = connect(Host, Port),

    State = #state{parent_pid=Parent, server_host=Host, server_port=Port, socket=Socket},
    {ok, State, ?SOCKET_TIMEOUT}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state(), Timeout :: integer() | infinity}.
handle_call(_Message, _From, State) ->
    {noreply, State, ?SOCKET_TIMEOUT}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state(), Timeout :: integer() | infinity}.
handle_cast({recv, <<0:16,Count:32>>}, State) ->
    #state{socket=Socket} = State,
    ok = gen_tcp:send(Socket, <<1:16, Count:32>>), 
    {noreply, State, ?SOCKET_TIMEOUT};

handle_cast({recv, _Packet}, State) ->
    {noreply, State, ?SOCKET_TIMEOUT};

handle_cast(tcp_closed, State) ->
    lager:debug("Connection to server lost.", []),
    {stop, {err, socket_closed}, State};

handle_cast({tcp_err, Reason}, State) ->
    lager:debug("Error reading from socket: ~p", [Reason]),
    {stop, {err, Reason}, State};

handle_cast(stop, _State) ->
    gen_server:cast(?MODULE, shutdown).

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state(), Timeout :: integer() | infinity}.
handle_info(_Message, State) ->
    {noreply, State, ?SOCKET_TIMEOUT}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, State) ->
    #state{socket=Socket} = State,
    gen_tcp:close(Socket),
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),
    spawn_link(fun() -> recv(Socket, self()) end),
    {ok, Socket}.

recv(Socket, Pid) ->
    Payload = case gen_tcp:recv(Socket, 0) of
        {ok, Packet} -> {tcp, Packet};
        {error, closed} -> tcp_closed;
        {error, Reason} -> {tcp_err, Reason}
    end,

    gen_server:cast(Pid, Payload),
    recv(Socket, Pid).

