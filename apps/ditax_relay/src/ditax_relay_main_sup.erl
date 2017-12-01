%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2017 14:22
%%%-------------------------------------------------------------------
-module(ditax_relay_main_sup).
-author("pravosudov").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    AChild = {acceptor_sup, {ditax_relay_acceptor_sup, start_link, []},
              Restart, Shutdown, Type, [ditax_relay_acceptor_sup]},

    AChild1 = {socket_controller_sup, {ditax_relay_controller_sup, start_link, []},
              Restart, Shutdown, Type, [ditax_relay_controller_sup]},

    AChild2 = {match_sup, {ditax_match_sup, start_link, []},
               Restart, Shutdown, Type, [ditax_match_sup]},

    {ok, {SupFlags, [AChild, AChild1, AChild2]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
