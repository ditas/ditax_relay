%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2017 13:48
%%%-------------------------------------------------------------------
-module(ditax_relay_controller).
-author("pravosudov").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    port,
    socket,
    acceptors = []
}).

-record(stored_acceptor, {
    module,
    pid,
    is_busy :: boolean()
}).

-define(TIMEOUT, 500).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Port]) ->
    AcceptorChildList = supervisor:which_children(ditax_relay_acceptor_sup),
    Acceptors = lists:foldl(fun({_Name, Pid, _Type, [Module]}, Acc) ->
        [#stored_acceptor{module = Module, pid = Pid, is_busy = false} | Acc]
                            end, [], AcceptorChildList),

    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),

    {ok, #state{port = Port, socket = Socket, acceptors = Acceptors}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({udp, Socket, RemoteIp, RemotePort, Data}, State) when State#state.socket =:= Socket ->

    io:format("------------Incoming Data ~p~n", [Data]),

    State1 = case get_acceptor(State#state.acceptors) of
        standby ->

            io:format("---------------1~n"),

            timer:send_after(?TIMEOUT, {awake, RemoteIp, RemotePort, Data}), %% TODO!
            State;
        #stored_acceptor{module = Module, pid = Pid} = Acceptor ->

            io:format("---------------2~n"),

            Module:accept(Pid, RemoteIp, RemotePort, Data),
            StoredAcceptors = lists:keydelete(Pid, #stored_acceptor.pid, State#state.acceptors),
            StoredAcceptors1 = [Acceptor#stored_acceptor{is_busy = true}|StoredAcceptors],
            State#state{acceptors = StoredAcceptors1}
    end,
    {noreply, State1};
handle_info({ok, From, HandlerPid}, State) ->

    ok = gen_udp:controlling_process(State#state.socket, HandlerPid),

    State1 = case lists:keyfind(From, #stored_acceptor.pid, State#state.acceptors) of
        #stored_acceptor{is_busy = true} = Acceptor ->

            io:format("---------------6~n"),

            StoredAcceptors = lists:keydelete(From, #stored_acceptor.pid, State#state.acceptors),
            StoredAcceptors1 = [Acceptor#stored_acceptor{is_busy = false}|StoredAcceptors],
            State#state{acceptors = StoredAcceptors1};
        _ ->
            State
    end,
    {noreply, State1};
handle_info(Info, State) ->

    io:format("------------Unknown message ~p~n", [Info]),

    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_acceptor([]) ->
    standby;
get_acceptor([#stored_acceptor{is_busy = false} = Acceptor|_]) ->
    Acceptor;
get_acceptor([#stored_acceptor{is_busy = true}|T]) ->
    get_acceptor(T).