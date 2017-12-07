%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Nov 2017 17:30
%%%-------------------------------------------------------------------
-module(ditax_player_handler).
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
    remote_ip,
    remote_port
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Data) ->

    io:format("---------------5~n"),

    gen_server:start_link(?MODULE, Data, []).

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
init({RemoteIp, RemotePort, Data}) ->

    io:format("------------ ~p ~p ~p~n", [RemoteIp, RemotePort, Data]),

    %% Предположим, что Data содержит данные в форматах
    %% от клиента-сервера: <<"{match:тип_матча, arena:арена, req_num:необходимое_кол-во_игроков, name:имя_игрока}">>
    %% от клиента: <<"{match:тип_матча, name:имя_игрока}">>

    self() ! {init, Data},

    %% TODO: может это нужно делать в контроллере?
    {ok, {out, Port}} = application:get_env(ditax_relay, outgoing),

    io:format("-------OUT Port ~p~n", [Port]),

    {ok, Socket} = gen_udp:open(Port, [binary, {active,true}]),

    io:format("-------OUT Socket ~p RemoteIp ~p RemotePort ~p~n", [Socket, RemoteIp, RemotePort]),

    ok = gen_udp:send(Socket, RemoteIp, RemotePort, integer_to_list(RemotePort)), %% Здесь будет какой-нибудь вразумительный ответ с контрольной суммой

    {ok, #state{remote_ip = RemoteIp, remote_port = RemotePort}}.

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
handle_info({init, Data}, State) ->
    Params = parse(Data),

    io:format("~p~n", [Params]),

    case lists:member(Params, <<"arena">>) of
        true ->
            %% Стартую процесс матча
            ditax_match_sup:start_child(MatchType, Arena, PlayersReqNumber, PlayerName);
        false ->
            %% Получаю список матчей, которые уже запущены
            Matches = supervisor:which_children(ditax_match_sup),

            io:format("--------MATCHES------- ~p~n", [Matches]),

            %% TODO: выбираю матч подходящий по типу. Делаю запрос к этому модулю на наличие мест в матче.
    end,

    {noreply, State};
handle_info(_Info, State) ->

    io:format("---------- IN ~p~n", [_Info]),

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
parse(Str) ->
    Str1 = re:replace(Str, "\\s+", "", [global,{return, list}]),
    Str2 = [E || E <- Str1, E =/= ${ andalso E =/= $\ andalso E =/= $" andalso E =/= $}],
    BinList = binary:split(list_to_binary(Str2), <<",">>, [global]),

    lists:foldl(fun(B, Acc) ->
        BL = binary:split(B, <<":">>, [global]),
        [list_to_tuple(BL)|Acc]
    end, [], BinList).