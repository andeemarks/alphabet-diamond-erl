-module(alphadiamond_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    alphadiamond_sup:start_link().

stop(_State) ->
    ok.

row_instructions_for(Letter) -> [Letter].

-ifdef(TEST).

basic_row_instructions_test() ->
	?assert(["A"] == row_instructions_for("A")).

% (fact (row-instructions-for "C") => [\A \B \C \B \A])

smoke_test() ->
    ok = application:start(alphadiamond).

-endif.