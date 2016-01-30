-module(alphadiamond_app).

-define(ROW_TEMPLATE, "ZYXWVUTSRQPONMLKJIHGFEDCBABCDEFGHIJKLMNOPQRSTUVWXYZ").
-define(ALPHABET, lists:seq($A,$Z)).

-export([diamond/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

everything_but(Letter) when is_integer(Letter) 	-> "[^" ++ io_lib:format("~c", [Letter]) ++ "]";
everything_but(Letter) 							-> "[^" ++ Letter ++ "]".

row_for(Letter) ->
	re:replace(?ROW_TEMPLATE, everything_but(Letter), " ", [global, {return,list}]).

row_instructions_for(Letter) when is_atom(Letter) -> 
	SubAlphabetEndPos = string:str(?ALPHABET, atom_to_list(Letter)),
	DiamondHalf = lists:sublist(?ALPHABET, SubAlphabetEndPos),
	lists:append(DiamondHalf, lists:reverse(lists:droplast(DiamondHalf))).

is_valid_spec(Spec) when is_atom(Spec) 		-> is_valid_spec(atom_to_list(Spec)); 
is_valid_spec(Spec) when is_integer(Spec) 	-> is_valid_spec(io_lib:format("~c", [Spec])); 
is_valid_spec(Spec) -> 
	TrimmedSpec = re:replace(Spec, " ", "", [global, {return, list}]),
	CleanSpec = re:replace(TrimmedSpec, "[^A-Za-z]", "", [global, {return,list}]),
	{(CleanSpec =:= TrimmedSpec) and (length(CleanSpec) == 1), list_to_atom(CleanSpec)}.

diamond() 							-> handle_invalid_spec().
diamond([]) 						-> handle_invalid_spec();
diamond([Spec|_]) 					-> diamond(Spec);
diamond(Spec) when is_atom(Spec) 	-> diamond(atom_to_list(Spec));
diamond(Spec) 						-> process_spec(is_valid_spec(Spec)).

process_spec({false, _}) 		-> handle_invalid_spec();
process_spec({true, ValidSpec}) ->
	Instructions = row_instructions_for(ValidSpec),
	lists:map(fun(Instruction) -> io:format(user, "\n~p", [row_for(Instruction)]) end, Instructions),
	ok.

handle_invalid_spec() ->
	io:format(user, "\nINVALID INPUT\n", []),
	error.	

-ifdef(TEST).

row_test_() -> [
	?_assertEqual("A", string:strip(row_for("A"))),
	?_assertEqual("B B", string:strip(row_for("B"))),
	?_assertEqual("J                 J", string:strip(row_for("J")))
].

row_instructions_test_() -> [
	?_assertEqual("A", row_instructions_for('A')),
	?_assertEqual("ABA", row_instructions_for('B')),
	?_assertEqual("ABCBA", row_instructions_for('C'))
].

status_after_spec_validation(Spec) ->
	{SpecStatus, _} = is_valid_spec(Spec),
	SpecStatus.

valid_spec_test_() -> [
	?_assert(status_after_spec_validation("A")),
	?_assert(status_after_spec_validation('A')),
	?_assert(status_after_spec_validation("c")),
	?_assert(status_after_spec_validation("  E   ")),
	?_assertNot(status_after_spec_validation("AA")),
	?_assertNot(status_after_spec_validation(" ")),
	?_assertNot(status_after_spec_validation("4")),
	?_assertNot(status_after_spec_validation("{")),
	?_assertNot(status_after_spec_validation(""))
].

positive_smoke_test_() -> [
    ?_assertEqual(ok, diamond(['A'])),
    ?_assertEqual(ok, diamond(["A"]))
].	

negative_smoke_test_() -> [
	?_assertEqual(error, diamond([";"])),
	?_assertEqual(error, diamond()),
	?_assertEqual(error, diamond([]))
].

-endif.