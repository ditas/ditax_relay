%%%-------------------------------------------------------------------
%%% @author pravosudov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2017 13:19
%%%-------------------------------------------------------------------
-author("pravosudov").

-record(acceptors, {
    number,
    module,
    params = []
}).