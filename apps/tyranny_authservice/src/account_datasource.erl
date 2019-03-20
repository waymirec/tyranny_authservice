%% Copyright (c) 2017-2023, Chris Waymire <chris@waymire.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(account_datasource).
-behavior(gen_server).

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-export([start/0,
  stop/0,
  get_account_by_username/1
]).

-record(state, {
  connection :: pid()
}).

-type state() :: #state{}.

-define(ACCOUNT_COLLECTION, <<"tyranny_account">>).

-spec start() -> {ok, pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
  gen_server:cast(?MODULE, stop).

-spec get_account_by_username(Username :: binary()) -> Account :: map().
get_account_by_username(Username) when is_binary(Username) ->
  gen_server:call(?MODULE, {get_account_by_username, Username}).

-spec init(Args :: list()) -> {ok, State :: state()}.
init([]) ->
  DbArgs = config:key(<<"account_db">>),
  {ok, Connection} = mc_worker_api:connect(DbArgs),
  State = #state{connection = Connection},
  {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: any()}, State :: state()) -> Result :: {reply, Reply :: term(), NewState :: state()}.
handle_call({get_account_by_username, Username}, _From, #state{connection = Connection} = State) ->
  Result = mc_worker_api:find_one(Connection, ?ACCOUNT_COLLECTION, #{<<"username">> => Username}, #{}),
  Status = maps:get(<<"status">>, Result),
  {reply, Result#{<<"status">> := trunc(Status)}, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_cast(stop, State) ->
  {stop, requested, State};

handle_cast(_Message, State) ->
  {noreply, State}.

-spec handle_info(Request :: term(), State :: state()) -> {noreply, State :: state()}.
handle_info(_Message, State) ->
  {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(OldVsn :: {down, term()} | term(), State :: gen_server:state(), Extra :: term()) -> {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

