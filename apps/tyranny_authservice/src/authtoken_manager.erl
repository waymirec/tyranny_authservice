-module(authtoken_manager).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-export([ create/2 ]).

-record(state, {
        db,
        token_ttl 
       }).

create(UserName, ServerIp) ->
    gen_server:call(?MODULE, {create, UserName, ServerIp}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    DbHost = config:key(<<"authtoken_db.host">>),
    DbPort = config:key(<<"authtoken_db.port">>),
    DbDatabase = config:key(<<"authtoken_db.database">>),
    DbPassword = config:key(<<"authtoken_db.password">>),
    ConnectTimeout = config:key(<<"authtoken_db.connect_timeout">>),
    ReconnectDelay = config:key(<<"authtoken_db.reconnect_delay">>),
    TokenTTL = config:key(<<"authtoken_db.token_ttl">>),
    {ok, DbClient} = eredis:start_link(DbHost, DbPort, DbDatabase, DbPassword, ReconnectDelay, ConnectTimeout),
    State = #state{db=DbClient, token_ttl=TokenTTL},
    {ok, State}.

handle_call({create, UserName, ServerIp}, _From, State) ->
    #state{db=DbClient, token_ttl=TTL} = State,
    UserNameLength = byte_size(UserName),
    ServerIpBin = inet_util:ip_to_int(ServerIp),
    Data = <<UserNameLength:8, UserName:UserNameLength/binary, ServerIpBin:32>>,
    Token = uuid:create(),
    lager:debug("Writing token [~p]: ~p", [Token, Data]),
    {ok, <<"OK">>} = eredis:q(DbClient, ["SET", Token, Data]),
    {ok, _} = eredis:q(DbClient, ["EXPIRE", Token, TTL]),
    {reply, Token, State};

handle_call(_Request, _From, State) -> {reply, ignored, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal functions
