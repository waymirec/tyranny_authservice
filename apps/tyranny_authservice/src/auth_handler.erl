-module(auth_handler).
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
	account = #{}				:: #{}
       }).

-type state() :: #state{}.

-spec start_link(Ref :: ranch:ref(), Socket :: gen_udp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    %% start the process syncronously to avoid deadlock
    {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, _Opts = []]) ->
    %% Perform any required state initialization here
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
    gen_statem:enter_loop(?MODULE, [], waiting_for_ident, #state{ref=Ref, socket=Socket, transport=Transport}).

-spec callback_mode() -> atom().
callback_mode() -> state_functions.

-spec waiting_for_ident(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_ident(info ,{tcp,_Port, <<MajorVsn:16, 
				      MinorVsn:16, 
				      MaintVsn:16,
				      Build:16,
				      UserNameLength:16,
				      Username:UserNameLength/binary>> = _RawData} = _Info, 
				      State) ->
    #state{transport=Transport, socket=Socket} = State,
    lager:debug("Client Ident (~p): Major=~p, Minor=~p, Maint=~p, Build=~p", [Username, MajorVsn, MinorVsn, MaintVsn, Build]),
    Challenge = crypto:hash(sha, Username),
    ChallengeLenBytes = byte_size(Challenge),
    lager:debug("Client Challenge (~p): ~p", [Username, Challenge]),
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
    Account = authservice_datasource:get_account_by_username(Username),
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
    #state{transport=Transport, socket=Socket, account=Account} = State,
    Status = maps:get(<<"status">>, Account),

    case Status of
        0 ->
	    {ok, ServerList} = auth_gamerouter:get_servers(),
	    [#game_server{ext_ip=ExtIp, ext_port=ExtPort} | _] = ServerList,
	    ExtIpBin = inet_util:ip_to_int(ExtIp),
	    Transport:send(Socket, <<0:32/integer, ExtIpBin:32/integer, ExtPort:32/integer>>);
        _ ->
	    Transport:send(Socket, <<Status:32/integer>>)
    end,
    Transport:setopts(State#state.socket, [{active,once}]),
    {stop, normal, State};

%	<<StatusBin/binary>> = <<Status:32/integer>>,
%	process_account_status(Account, StatusBin, State),
%	case <<Status:32>> of
%	    <<0:32>> ->
%		lager:debug("user is good to go.", []),
%		{ip=Ip,port=Port} = authservice_lb:get_server(),
%	 	Transport:send(Socket, <<Status:32,Ip:32,Port:32>>);
%	    <<1:1, _Rest/bitstring>> ->
%		lager:debug("do not pass go.", []),
%		Transport:send(Socket, <<Status:32>>)
%	end,

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
    #state{challenge=Challenge, proof=Proof, username=Username}=State,
    PasswordHash = base64:decode(PasswordHashEncoded),
    ProofContext1 = crypto:hash_init(sha256),
    ProofContext2 = crypto:hash_update(ProofContext1, Challenge),
    ProofContext3 = crypto:hash_update(ProofContext2, PasswordHash),
    ServerProof  = crypto:hash_final(ProofContext3),
    lager:debug("Client Proof (~p): Client=~p, Server=~p", [Username, Proof, ServerProof]),
    validate_proof(Proof, ServerProof, State);

auth_user(undefined, State) ->
    #state{transport=Transport, socket=Socket, username=Username} = State,
    lager:debug("Unknown username: ~p", [Username]),
    Transport:send(Socket, <<0:8>>),
    {err, unknown_username};

auth_user(Response, State) ->
    #state{transport=Transport, socket=Socket, username=Username} = State,
    lager:debug("Unexpected response retrieving user '~p': ~p", [Username, Response]),
    Transport:send(Socket, <<0:8>>),
    {err, unexpected_response}.

-spec validate_proof(Proof1 :: binary(), Proof2 :: binary(), State :: state()) -> ok | {err, atom()}.
validate_proof(Proof, Proof, State) ->
    #state{transport=Transport, socket=Socket, username=Username} = State,
    lager:debug("Auth Success: ~p", [Username]),
    Transport:send(Socket, <<1:8>>),
    ok;

validate_proof(_, _, State) ->
    #state{transport=Transport, socket=Socket}=State,
    lager:debug("Auth failed. Invalid password.", []),
    Transport:send(Socket, <<0:8>>),
    {err, invalid_pasword}.

%process_account_status(Account, <<"0">>=Status, State) ->
%    lager:debug("user is good to go.", []),
%    ok;
%
%process_account_status(Account, <<_:31/bitstring,1:1>>=Status, State) ->
%    lager:debug("first set.", []),
%    ok;
%
%process_account_status(Account, <<_:30/bitstring,1:1,_/bitstring>>=Status, State) ->
%    lager:debug("second set.", []),
%    ok;
%
%process_account_status(Account, <<_:29/bitstring,1:1,_/bitstring>>=Status, State) ->
%    lager:debug("third set.", []),
%    ok.
