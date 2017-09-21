-module(gameservice_client).
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
         start_link/1,
         stop/0
        ]).

-record(state, {
	server_host = <<"">>			:: inet:socket_address() | inet:hostname(),
	server_port = 0				:: integer(),
	socket					:: gen_tcp:socket()
       }).

-type state() :: #state{}.

-define(SOCKET_TIMEOUT, 5000).

-spec start_link(list()) -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link([Host, Port]) when is_integer(Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec init(Args :: list()) -> {ok, State :: state(), Timeout :: integer() | infinity}.
init([Host, Port]) ->
    {ok, Socket} = connect(Host, Port),

    State = #state{server_host=Host, server_port=Port, socket=Socket},
    {ok, State, ?SOCKET_TIMEOUT}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state(), Timeout :: integer() | infinity}.
handle_call(_Message, _From, State) ->
    {noreply, State, ?SOCKET_TIMEOUT}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state(), Timeout :: integer() | infinity}.
handle_cast(stop, State) ->
    {stop, requested, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state(), Timeout :: integer() | infinity}.
handle_info({tcp, Socket, <<0:32>>}, State) ->
    #state{socket=Socket} = State,
    lager:debug("Ping!", []),
    gen_tcp:send(Socket, <<1:32>>),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?SOCKET_TIMEOUT};

handle_info({tcp, Socket, Packet}, State) ->
    #state{socket=Socket} = State,
    lager:debug("Ignoring packet: ~p", [Packet]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?SOCKET_TIMEOUT};

handle_info({tcp_closed, _Socket}, State) ->
    lager:debug("Connection to server lost.", []),
    {stop, {err, socket_closed}, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(Message, State) ->
    #state{socket=Socket} = State,
    lager:debug("Got Message: ~p", [Message]),
    inet:setopts(Socket, [{active, once}]),
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
    lager:debug("Opening socket to ~p/~p", [Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, once}]),
    lager:debug("Socket == ~p", [Socket]),
    %spawn_link(fun() -> recv(Socket, self()) end),
    {ok, Socket}.

