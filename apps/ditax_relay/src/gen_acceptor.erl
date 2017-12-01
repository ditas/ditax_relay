%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2017 13:40
%%%-------------------------------------------------------------------
-module(gen_acceptor).
-author("pravosudov").

-callback start_link(term()) -> {ok, pid()}.
-callback stop(pid()) -> ok.
-callback accept(term(), term()) -> ok.

%% API
-export([]).
