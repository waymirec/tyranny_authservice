-module(authservice_handler).
-behavior(gen_statem).
-behavior(ranch_protocol).

%% Ranch Callbacks
-export([ start_link/4,
	  init/1
        ]).

%% GEN_FSM Callbacks
-export([ terminate/3, 
	  code_change/4, 
	  callback_mode/0
	]).

%% State callbacks
-export([ waiting_for_ident/3,
	  waiting_for_proof/3,
	  waiting_for_ack/3
	]).

-include("authservice.hrl").

-record(state, {
  	ref					:: ranch:ref(), % provided by ranch
	socket					:: gen_udp:socket(), % udp socket
	transport				:: module(), % ranch Transport (tcp, udp, ssl, etc) 
	username = <<"">>			:: binary(), 
	secret = <<"">>				:: binary(), 
	challenge = <<"">>			:: binary(),
	proof = <<"">>				:: binary(),
	account = #{}				:: #{},
	target_server				:: inet:ip_address()
       }).

-type state() :: #state{}.

-spec start_link(Ref :: ranch:ref(), Socket :: gen_udp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    %% start the process syncronously to avoid deadlock
    {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, _Opts = []]) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
    gen_statem:enter_loop(?MODULE, [], waiting_for_ident, #state{ref=Ref, socket=Socket, transport=Transport}).

-spec callback_mode() -> atom().
callback_mode() -> state_functions.

-spec waiting_for_ident(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_ident(info ,{tcp,_Port, <<_MajorVsn:16, 
				      _MinorVsn:16, 
				      _MaintVsn:16,
				      _Build:16,
				      UserNameLength:16,
				      Username:UserNameLength/binary>> = _RawData} = _Info, 
				      State) ->
    #state{transport=Transport, socket=Socket} = State,
    Challenge = crypto:hash(sha, Username),
    ChallengeLenBytes = byte_size(Challenge),
    NewState = State#state{username=Username, secret= <<"foo">>, challenge=Challenge},
    Transport:send(Socket, <<ChallengeLenBytes:16, Challenge/binary>>),
    Transport:setopts(State#state.socket, [{active, once}]),
    {next_state, waiting_for_proof, NewState};

waiting_for_ident(info, {tcp_closed, _Port}, State) ->
    socket_closed(waiting_for_ident, State),
    {stop, {err, socket_closed}, State};

waiting_for_ident(info, {tcp, _Port, RawData}, State) ->
    {stop, {err, {unexpected, RawData}}, State}.

-spec waiting_for_proof(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_proof(info, {tcp,_Port,<<ProofLen:16, Proof:ProofLen/binary>> = _RawData} = _Info, State) ->
    #state{transport=Transport, socket=Socket, username=Username} = State,
    NewState = State#state{proof=Proof},
    Account = account_datasource:get_account_by_username(Username),
    auth_user(Account, NewState),
    Transport:setopts(Socket, [{active, once}]),
    {next_state, waiting_for_ack, NewState#state{account=Account}};

waiting_for_proof(info, {tcp_closed, _Port}, State) ->
    socket_closed(waiting_for_proof, State),
    {stop, {err, socket_closed}, State};

waiting_for_proof(info, {tcp, _Port, RawData}, State) ->
    {stop, {err, {unexpected, RawData}}, State}.

-spec waiting_for_ack(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_ack(info, {tcp, _Port, <<1:32/integer>> = _RawData}, State) ->
    #state{transport=Transport, socket=Socket, account=Account, username=UserName} = State,
    Status = maps:get(<<"status">>, Account),

    case Status of
        0 ->
	    ServerList = gameservice_finder:list(),
	    [#server_info{ip=Ip, port=Port} | _] = ServerList,
	    AuthToken = authtoken_manager:create(UserName, Ip),
	    AuthTokenLen = bit_size(AuthToken),
	    IpBin = inet_util:ip_to_int(Ip),
	    Transport:send(Socket, <<0:32/integer, IpBin:32/integer, Port:32/integer, AuthTokenLen:16, AuthToken:AuthTokenLen/bitstring>>);
        _ ->
	    Transport:send(Socket, <<Status:32/integer>>)
    end,
    Transport:setopts(State#state.socket, [{active,once}]),
    {stop, normal, State};

waiting_for_ack(info, {tcp_closed, _Port}, State) ->
    socket_closed(waiting_for_ready, State),
    {stop, {err, socket_closed}, State};

waiting_for_ack(info, {tcp, _Port, RawData}, State) ->
    {stop, {unexpected_ready, RawData}, State}.

-spec socket_closed(StateName :: binary() | list(), State :: state()) -> ok.
socket_closed(StateName, State) ->
    #state{transport=Transport, socket=Socket} = State,
    lager:debug("StateName=~p, Connection closed by remote side.", [StateName]),
    lager:debug("Transport: ~p, Socket: ~p", [Transport, Socket]),
    Transport:close(Socket),
    ok.

-spec terminate( Reason :: normal | shutdown | {shutdown,term()} | term(), StateName :: gen_statem:state(), State :: state()) -> ok.
terminate(_Reason, _StateName, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()}, OldState :: term(), OldData :: gen_statem:state(), Extra :: term()) -> {ok, term(), gen_statem:data()} | term().
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

-spec auth_user(Account :: map(), State :: state()) -> Ignored :: any().
auth_user(#{<<"passwordHash">> := PasswordHashEncoded} = _Record, State) ->
    #state{challenge=Challenge, proof=Proof}=State,
    PasswordHash = base64:decode(PasswordHashEncoded),
    ProofContext1 = crypto:hash_init(sha256),
    ProofContext2 = crypto:hash_update(ProofContext1, Challenge),
    ProofContext3 = crypto:hash_update(ProofContext2, PasswordHash),
    ServerProof  = crypto:hash_final(ProofContext3),
    validate_proof(Proof, ServerProof, State);

auth_user(undefined, State) ->
    #state{transport=Transport, socket=Socket} = State,
    Transport:send(Socket, <<0:8>>),
    {err, unknown_username};

auth_user(Response, State) ->
    #state{transport=Transport, socket=Socket, username=Username} = State,
    lager:debug("Unexpected response retrieving user '~p': ~p", [Username, Response]),
    Transport:send(Socket, <<0:8>>),
    {err, unexpected_response}.

-spec validate_proof(Proof1 :: binary(), Proof2 :: binary(), State :: state()) -> ok | {err, atom()}.
validate_proof(Proof, Proof, State) ->
    #state{transport=Transport, socket=Socket} = State,
    Transport:send(Socket, <<1:8>>),
    ok;

validate_proof(_, _, State) ->
    #state{transport=Transport, socket=Socket}=State,
    Transport:send(Socket, <<0:8>>),
    {err, invalid_pasword}.

