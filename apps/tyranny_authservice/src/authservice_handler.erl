-module(authservice_handler).
-behavior(gen_statem).
-behavior(ranch_protocol).

%% Ranch Callbacks
-export([start_link/4,
  init/1
]).

%% GEN_FSM Callbacks
-export([terminate/3,
  code_change/4,
  callback_mode/0
]).

%% State callbacks
-export([waiting_for_ident/3,
  waiting_for_proof/3,
  waiting_for_ack/3,
  pending_close/3
]).

-include("authservice.hrl").

-record(state, {
  ref :: ranch:ref(), % provided by ranch
  client_id :: binary(),
  socket :: gen_udp:socket(), % udp socket
  transport :: module(), % ranch Transport (tcp, udp, ssl, etc)
  username = <<"">> :: binary(),
  challenge = <<"">> :: binary(),
  proof = <<"">> :: binary(),
  account = #{} :: #{},
  target_server :: inet:ip_address()
}).

-type state() :: #state{}.

-spec start_link(Ref :: ranch:ref(), Socket :: gen_udp:socket(), Transport :: module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  %% start the process syncronously to avoid deadlock
  {ok, proc_lib:spawn_link(?MODULE, init, [[Ref, Socket, Transport, Opts]])}.

-spec init(Args :: list()) -> no_return().
init([Ref, Socket, Transport, _Opts = []]) ->
  {ok, {IpAddress, Port}} = inet:peername(Socket),
  ClientId = list_to_binary(io_lib:format("~s:~p", [inet:ntoa(IpAddress), Port])),
  lager:info("[~s] Connection accepted from client ~s:~p", [ClientId, inet:ntoa(IpAddress), Port]),
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),
  gen_statem:enter_loop(?MODULE, [], waiting_for_ident, #state{ref = Ref, socket = Socket, transport = Transport, client_id = ClientId}).

-spec callback_mode() -> atom().
callback_mode() -> state_functions.

-spec waiting_for_ident(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_ident(info, {tcp, _Port, <<?OP_IDENT,
  MajorVsn:16,
  MinorVsn:16,
  MaintVsn:16,
  Build:16,
  UserNameLength:16,
  Username:UserNameLength/binary>> = _RawData} = _Info,
    State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket} = State,
  lager:debug("[~s] Received ident: Major=[~p], Minor=[~p], Maint=[~p], Build=[~p], Username=[~p]", [ClientId, MajorVsn, MinorVsn, MaintVsn, Build, Username]),
  Challenge = crypto:hash(sha, uuid:create()),
  ChallengeLenBytes = byte_size(Challenge),
  lager:debug("[~s] Generated challenge: ~s", [ClientId, [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Challenge]]),
  NewState = State#state{username = Username, challenge = Challenge},

  Transport:send(Socket, <<?OP_CHALLENGE, ChallengeLenBytes:16, Challenge/binary>>),
  Transport:setopts(State#state.socket, [{active, once}]),
  {next_state, waiting_for_proof, NewState};

waiting_for_ident(info, {tcp_closed, _Port}, #state{client_id = ClientId} = State) ->
  lager:info("[~s] Socket closed.", [ClientId]),
  socket_closed(waiting_for_ident, State),
  {stop, {err, socket_closed}, State};

waiting_for_ident(info, {tcp, _Port, RawData}, #state{client_id = ClientId} = State) ->
  lager:info("[~s] Received unexpected data: ~p.", [ClientId, RawData]),
  {stop, {err, {unexpected, RawData}}, State}.

-spec waiting_for_proof(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_proof(info, {tcp, _Port, <<?OP_PROOF, ProofLen:16, Proof:ProofLen/binary>> = _RawData} = _Info, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket, username = Username} = State,
  lager:debug("[~s] Received proof: ~s", [ClientId, [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= Proof]]),
  NewState = State#state{proof = Proof},
  Account = account_datasource:get_account_by_username(Username),

  case auth_user(Account, NewState) of
    ok ->
      lager:debug("[~s] User successfully authenticated", [ClientId]),
      Transport:setopts(Socket, [{active, once}]),
      {next_state, waiting_for_ack, NewState#state{account = Account}};
    {err, Reason} ->
      lager:debug("[~s] User failed to authenticate: ~s", [ClientId, Reason]),
      timer:send_after(2000, self(), {err, Reason}),
      {next_state, pending_close, NewState}
  end;

waiting_for_proof(info, {tcp_closed, _Port}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Socket closed waiting for proof", [ClientId]),
  socket_closed(waiting_for_proof, State),
  {stop, {err, socket_closed}, State};

waiting_for_proof(info, {tcp, _Port, RawData}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Received unexpected data while waiting for proof: ~p", [ClientId, RawData]),
  {stop, {err, {unexpected, RawData}}, State}.

-spec waiting_for_ack(info, tuple(), State :: state()) -> gen_statem:state_callback_result(gen_statem:action()).
waiting_for_ack(info, {tcp, _Port, <<?OP_PROOF_ACK_ACK, 1:32/integer>> = _RawData}, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket, account = Account, username = UserName} = State,
  lager:debug("[~s] Received ack from client", [ClientId]),
  Status = maps:get(<<"status">>, Account),

  lager:debug("[~s] Account status is '~p'", [ClientId, Status]),

  case Status of
    0 ->
      case gameservice_finder:list() of
        [#server_info{ip = Ip, port = Port} | _] ->
          AuthToken = authtoken_manager:create(UserName, Ip),
          lager:debug("[~s] Generated auth token [~s] for server [~s]", [ClientId, AuthToken, inet_util:ip_to_bin(Ip)]),
          AuthTokenLen = byte_size(AuthToken),
          IpBin = inet_util:ip_to_int(Ip),
          Transport:send(Socket, <<?OP_AUTH_CMP, 0:32/integer, IpBin:32/integer, Port:32/integer, AuthTokenLen:16, AuthToken/binary>>);
        [] ->
          Transport:send(Socket, <<?OP_AUTH_CMP, 999:32/integer>>)
      end;
    _ ->
      Transport:send(Socket, <<?OP_AUTH_CMP, Status:32/integer>>)
  end,
  Transport:setopts(State#state.socket, [{active, once}]),
  {stop, normal, State};

waiting_for_ack(info, {tcp_closed, _Port}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Socket closed waiting for ack", [ClientId]),
  socket_closed(waiting_for_ack, State),
  {stop, {err, socket_closed}, State};

waiting_for_ack(info, {tcp, _Port, RawData}, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Received unexpected data while waiting for ack: ~p", [ClientId, RawData]),
  {stop, {unexpected, RawData}, State}.

pending_close(info, Reason, State) ->
  {stop, Reason, State}.

-spec socket_closed(StateName :: binary() | list(), State :: state()) -> ok.
socket_closed(StateName, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket} = State,
  lager:debug("[~s] StateName=~p, Connection closed by remote side.", [ClientId, StateName]),
  lager:debug("[~s] Transport: ~p, Socket: ~p", [ClientId, Transport, Socket]),
  Transport:close(Socket),
  ok.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), StateName :: gen_statem:state(), State :: state()) -> ok.
terminate(_Reason, _StateName, #state{client_id = ClientId} = State) ->
  lager:debug("[~s] Terminating handler", [ClientId]),
  ok.

-spec code_change(OldVsn :: term() | {down, term()}, OldState :: term(), OldData :: gen_statem:state(), Extra :: term()) -> {ok, term(), gen_statem:data()} | term().
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec auth_user(Account :: map(), State :: state()) -> Ignored :: any().
auth_user(#{<<"passwordHash">> := PasswordHashEncoded} = _Record, State) ->
  #state{client_id = ClientId, challenge = Challenge, proof = Proof} = State,
  PasswordHash = hex_to_bin(binary_to_list(base64:decode(PasswordHashEncoded))),
  ProofContext1 = crypto:hash_init(sha256),
  ProofContext2 = crypto:hash_update(ProofContext1, Challenge),
  ProofContext3 = crypto:hash_update(ProofContext2, PasswordHash),
  ServerProof = crypto:hash_final(ProofContext3),
  lager:debug("[~s] Generated server proof: ~s", [ClientId, [io_lib:format("~2.16.0B", [X]) || <<X:8>> <= ServerProof]]),
  validate_proof(Proof, ServerProof, State);

auth_user(undefined, State) ->
  #state{transport = Transport, socket = Socket} = State,
  Transport:send(Socket, <<?OP_PROOF_ACK, 1:8>>),
  {err, unknown_username};

auth_user(Response, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket, username = Username} = State,
  lager:debug("[~s] Unexpected response retrieving user '~p': ~p", [ClientId, Username, Response]),
  Transport:send(Socket, <<?OP_PROOF_ACK, 1:8>>),
  {err, unexpected_response}.

-spec validate_proof(Proof1 :: binary(), Proof2 :: binary(), State :: state()) -> ok | {err, atom()}.
validate_proof(Proof, Proof, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket} = State,
  lager:debug("[~s] Password is valid", [ClientId]),
  Transport:send(Socket, <<?OP_PROOF_ACK, 0:8>>),
  ok;

validate_proof(_, _, State) ->
  #state{client_id = ClientId, transport = Transport, socket = Socket} = State,
  lager:debug("[~s] Password is invalid", [ClientId]),
  Transport:send(Socket, <<?OP_PROOF_ACK, 1:8>>),
  {err, invalid_pasword}.


hex_to_bin(S) ->
  hex_to_bin(S, []).

hex_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));

hex_to_bin([X, Y | T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hex_to_bin(T, [V | Acc]).