-module(config).
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
	  start_link/0,
	  stop/0,
	  port/0,
	  num_acceptors/0,
	  max_connections/0,
	  hello_addr/0,
	  hello_port/0,
	  hello_interval/0
        ]).

-record(state, {
        config = #{}			:: map()
       }).

-type state() :: #state{}.

-define(DEF_PORT, 5555).
-define(DEF_NUM_ACCEPTORS, 10).
-define(DEF_MAX_CONNECTIONS, 10).
-define(DEF_HELLO_ADDR, {224,0,0,100}).
-define(DEF_HELLO_PORT, 12345).
-define(DEF_HELLO_INTERVAL, 1000).

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() -> gen_server:cast(?MODULE, stop).

-spec port() -> term().
port() -> gen_server:call(?MODULE, port).

-spec num_acceptors() -> term().
num_acceptors() -> gen_server:call(?MODULE, num_acceptors).

-spec max_connections() -> term().
max_connections() -> gen_server:call(?MODULE, max_connections).

-spec hello_addr() -> term().
hello_addr() -> gen_server:call(?MODULE, hello_addr).

-spec hello_port() -> term().
hello_port() -> gen_server:call(?MODULE, hello_port).

-spec hello_interval() -> term().
hello_interval() -> gen_server:call(?MODULE, hello_interval).

-spec init(Args :: list()) -> {ok, State :: state()}.
init([]) ->
    Config = maps:from_list(application:get_all_env(tyranny_authservice)),
    State = #state{config = Config},
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call(port, _From, #state{config=Config}=State) ->
    {reply, maps:get(port, Config, ?DEF_PORT), State};

handle_call(num_acceptors, _From, #state{config=Config}=State) ->
    {reply, maps:get(num_acceptors, Config, ?DEF_NUM_ACCEPTORS), State};

handle_call(max_connections, _From, #state{config=Config}=State) ->
    {reply, maps:get(max_connections, Config, ?DEF_MAX_CONNECTIONS), State};

handle_call(hello_addr, _From, #state{config=Config}=State) ->
    {reply, maps:get(hello_addr, Config, ?DEF_HELLO_ADDR), State};

handle_call(hello_port, _From, #state{config=Config}=State) ->
    {reply, maps:get(hello_port, Config, ?DEF_HELLO_PORT), State};

handle_call(hello_interval, _From, #state{config=Config}=State) ->
    {reply, maps:get(hello_interval, Config, ?DEF_HELLO_INTERVAL), State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(stop, State) ->
    {stop, requested, State}.

handle_info(_Message, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
