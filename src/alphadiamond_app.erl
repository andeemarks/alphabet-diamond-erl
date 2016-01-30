-module(alphadiamond_app).

-behaviour(application).

-define(ROW_TEMPLATE, "ZYXWVUTSRQPONMLKJIHGFEDCBABCDEFGHIJKLMNOPQRSTUVWXYZ").

%% Application callbacks
-export([diamond/1, start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(_StartType, _StartArgs) ->
    alphadiamond_sup:start_link().

stop(_State) ->
    ok.

row_instructions_for(Letter) -> 
	Alphabet = lists:seq($A,$Z),
	SubAlphabetEndPos = string:str(Alphabet, Letter),
	DiamondHalf = lists:sublist(Alphabet, SubAlphabetEndPos),
	% io:format(user, "\n~p ~p\n", [Letter, DiamondHalf]),
	lists:append(DiamondHalf, lists:reverse(lists:droplast(DiamondHalf))).

everything_but(Letter) when is_integer(Letter) -> 	"[^" ++ io_lib:format("~c", [Letter]) ++ "]";
everything_but(Letter) -> 							"[^" ++ Letter ++ "]".

row_for(Letter) ->
	re:replace(?ROW_TEMPLATE, everything_but(Letter), " ", [global, {return,list}]).

is_valid_spec(Spec) when is_atom(Spec) -> is_valid_spec(atom_to_list(Spec)); 
is_valid_spec(Spec) when is_integer(Spec) -> is_valid_spec(io_lib:format("~c", [Spec])); 
is_valid_spec(Spec) -> 
	TrimmedSpec = re:replace(Spec, " ", "", [global, {return, list}]),
	CleanSpec = re:replace(TrimmedSpec, "[^A-Za-z]", "", [global, {return,list}]),
	(CleanSpec =:= TrimmedSpec) and (length(CleanSpec) == 1).

diamond([Spec|_]) -> 
	diamond(Spec);
diamond([]) -> 
	io:format(user, "\nINVALID INPUT\n", []),
	false;
diamond(Spec) when is_atom(Spec) -> 
	diamond(atom_to_list(Spec));
diamond(Spec) ->
	ValidSpec = is_valid_spec(Spec),
    if
    	ValidSpec ->
			Instructions = row_instructions_for(lists:nth(1, io_lib:format("~c", [Spec]))),
			lists:map(fun(Instruction) -> io:format(user, "\n~p", [row_for(Instruction)]) end, Instructions),
			true;
		true ->
			io:format(user, "\nINVALID INPUT\n", []),
			false
	end.

diamond() -> 
	io:format(user, "\nINVALID INPUT\n", []),
	false.	

-ifdef(TEST).

row_test_() -> [
	?_assertEqual("A", string:strip(row_for("A"))),
	?_assertEqual("B B", string:strip(row_for("B"))),
	?_assertEqual("J                 J", string:strip(row_for("J")))
].

row_instructions_test_() -> [
	?_assertEqual("A", row_instructions_for("A")),
	?_assertEqual("ABA", row_instructions_for("B")),
	?_assertEqual("ABCBA", row_instructions_for("C"))
].

valid_spec_test_() -> [
	?_assert(is_valid_spec("A")),
	?_assert(is_valid_spec('A')),
	?_assert(is_valid_spec("c")),
	?_assert(is_valid_spec("  E   ")),
	?_assertNot(is_valid_spec("AA")),
	?_assertNot(is_valid_spec(" ")),
	?_assertNot(is_valid_spec("4")),
	?_assertNot(is_valid_spec("{")),
	?_assertNot(is_valid_spec(""))
].

positive_smoke_test_() -> [
    ?_assert(diamond(['A'])),
    ?_assert(diamond(["A"]))
].	

negative_smoke_test_() -> [
	?_assertNot(diamond([";"])),
	?_assertNot(diamond()),
	?_assertNot(diamond([]))
].

-endif.