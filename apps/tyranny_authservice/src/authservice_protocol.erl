-module(authservice_protocol).
-behavior(gen_statem).
-behavior(ranch_protocol).

-export([start_link/4]).
-export([init/1]).

%% GEN_FSM Callbacks
-export([ terminate/3, 
	  code_change/4, 
	  callback_mode/0
	]).

-export([ waiting_for_ident/3,
	  waiting_for_proof/3,
	  waiting_for_ready/3
	]).

-record(state, {
	  	ref, 
		socket, 
		transport, 
		username, 
		secret, 
		challenge,
		proof
	       }).

start_link(Ref, Socket, Transport, Opts) ->
	%% start the process syncronously to avoid deadlock
	{ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

init([Ref, Socket, Transport, _Opts = []]) ->
	%% Perform any required state initialization here
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
        gen_statem:enter_loop(?MODULE, [], waiting_for_ident, #state{ref=Ref, socket=Socket, transport=Transport}).

callback_mode() -> state_functions.

waiting_for_ident(info ,{tcp,_Port, <<MajorVsn:16,MinorVsn:16,MaintVsn:16,Build:16,UserNameLength:16,Username:UserNameLength/binary>> = _RawData} = _Info, #state{transport=Transport,socket=Socket}=State) ->
	lager:debug("Client Ident (~p): Major=~p, Minor=~p, Maint=~p, Build=~p", [Username, MajorVsn, MinorVsn, MaintVsn, Build]),
	Challenge = crypto:hash(sha, Username),
	ChallengeLenBytes = byte_size(Challenge),
	lager:debug("Client Challenge (~p): ~p", [Username, Challenge]),
	NewState = State#state{username=Username, secret= <<"foo">>, challenge=Challenge},
	Transport:send(Socket, <<ChallengeLenBytes:16,Challenge/binary>>),
        Transport:setopts(State#state.socket, [{active,once}]),
	{next_state, waiting_for_proof, NewState};

waiting_for_ident(info, {tcp_closed, _Port}, State) ->
	socket_closed(waiting_for_ident, State),
	{stop, {err, socket_closed}, State};

waiting_for_ident(info, {tcp, _Port, RawData}, State) ->
	{stop, {err, {unexpected, RawData}}, State}.

waiting_for_proof(info, {tcp,_Port,<<ProofLen:16, Proof:ProofLen/binary>> = _RawData} = _Info, #state{transport=Transport,socket=Socket, challenge=Challenge, username=Username}=State) ->
	NewState = State#state{proof=Proof},
	Account = authservice_datasource:get_account_by_username(Username),
	auth_user(Account, NewState),
        Transport:setopts(State#state.socket, [{active,once}]),
	{next_state, waiting_for_ready, NewState};

waiting_for_proof(info, {tcp_closed, _Port}, State) ->
	socket_closed(waiting_for_proof, State),
	{stop, {err, socket_closed}, State};

waiting_for_proof(info, {tcp, _Port, RawData}, State) ->
	{stop, {err, {unexpected, RawData}}, State}.

waiting_for_ready(info, {tcp,_Port,_RawData}, #state{transport=Transport,socket=Socket}=State) ->
	lager:debug("Got Ready.", []),
	Transport:send(Socket, msgpack:pack(<<"got ready">>)),
        Transport:setopts(State#state.socket, [{active,once}]),
	{next_state, waiting_for_ready, State};

waiting_for_ready(info, {tcp_closed, _Port}, State) ->
	socket_closed(waiting_for_ready, State),
	{stop, {err, socket_closed}, State};

waiting_for_ready(info, {tcp, _Port, RawData}, State) ->
	{stop, {unexpected_ready, RawData}, State}.

socket_closed(StateName, #state{transport=Transport, socket=Socket} = _State) ->
	lager:debug("StateName=~p, Connection closed by remote side.", [StateName]),
	lager:debug("Transport: ~p, Socket: ~p", [Transport, Socket]),
	Transport:close(Socket),
	ok.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

auth_user(#{<<"passwordHash">> := PasswordHashEncoded, <<"status">> := Status} = _Record,  #state{transport=Transport}=State) ->
	PasswordHash = base64:decode(PasswordHashEncoded),
    	ProofContext1 = crypto:hash_init(sha256),
      	ProofContext2 = crypto:hash_update(ProofContext1, State#state.challenge),
        ProofContext3 = crypto:hash_update(ProofContext2, PasswordHash),
	ServerProof  = crypto:hash_final(ProofContext3),
	lager:debug("Client Proof (~p): Client=~p, Server=~p", [State#state.username, State#state.proof, ServerProof]),
	case State#state.proof of
		ServerProof -> 
			lager:debug("Auth Success: ~p", [State#state.username]),
        		Transport:send(State#state.socket, <<1:8>>),
			ok;
	         _ -> 
			lager:debug("Auth failed. Invalid password.", []),
        		Transport:send(State#state.socket, <<0:8>>),
			{err, invalid_pasword}
	end;

auth_user(undefined, #state{transport=Transport} = State) ->
	lager:debug("Unknown username: ~p", [State#state.username]),
        Transport:send(State#state.socket, <<0:8>>),
	{err, unknown_username};

auth_user(Response, #state{transport=Transport} = State) ->
	lager:debug("Unexpected response retrieving user '~p': ~p", [State#state.username, Response]),
        Transport:send(State#state.socket, <<0:8>>),
	{err, unexpected_response}.
