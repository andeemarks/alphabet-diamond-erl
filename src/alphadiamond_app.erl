-module(alphadiamond_app).

-behaviour(application).

-define(ROW_TEMPLATE, "ZYXWVUTSRQPONMLKJIHGFEDCBABCDEFGHIJKLMNOPQRSTUVWXYZ").

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

row_instructions_for(Letter) -> 
	Alphabet = lists:seq($A,$Z),
	SubAlphabetEndPos = string:str(Alphabet, Letter),
	DiamondHalf = lists:sublist(Alphabet, SubAlphabetEndPos),
	lists:append(DiamondHalf, lists:reverse(lists:droplast(DiamondHalf))).

everything_but(Letter) ->
	"[^" ++ Letter ++ "]".

row_for(Letter) ->
	re:replace(?ROW_TEMPLATE, everything_but(Letter), " ", [global, {return,list}]).

-ifdef(TEST).

row_test_() ->
	[
		?_assertEqual("A", string:strip(row_for("A"))),
		?_assertEqual("B B", string:strip(row_for("B"))),
		?_assertEqual("J                 J", string:strip(row_for("J")))
	].

row_instructions_test_() ->
	[
		?_assertEqual("A", row_instructions_for("A")),
		?_assertEqual("ABA", row_instructions_for("B")),
		?_assertEqual("ABCBA", row_instructions_for("C"))
	].

valid_spec_test_() ->
	[
		?_assertTrue(is_valid_spec("A")),
		?_assertFalse(is_valid_spec("*"))
	].

% smoke_test() ->
%     ok = application:start(alphadiamond).

-endif.