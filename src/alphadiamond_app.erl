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

-ifdef(TEST).

simple_test() ->
    ok = application:start(alphadiamond),
    ?assertNot(undefined == whereis(alphadiamond_app)).

-endif.