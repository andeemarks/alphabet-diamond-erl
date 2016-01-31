-module(alphadiamond_app_tests).

-include_lib("eunit/include/eunit.hrl").

positive_smoke_test_() -> [
    ?_assertEqual(ok, alphadiamond_app:diamond(['A'])),
    ?_assertEqual(ok, alphadiamond_app:diamond(["A"]))
].	

negative_smoke_test_() -> [
	?_assertEqual(error, alphadiamond_app:diamond([";"])),
	?_assertEqual(error, alphadiamond_app:diamond()),
	?_assertEqual(error, alphadiamond_app:diamond([]))
].

row_after_strip(Letter) -> string:strip(alphadiamond_app:row_for(Letter)).

row_test_() -> [
	?_assertEqual("A", row_after_strip("A")),
	?_assertEqual("B B", row_after_strip("B")),
	?_assertEqual("J                 J", row_after_strip("J"))
].

row_instructions_test_() -> [
	?_assertEqual("A", alphadiamond_app:row_instructions_for('A')),
	?_assertEqual("ABA", alphadiamond_app:row_instructions_for('B')),
	?_assertEqual("ABCBA", alphadiamond_app:row_instructions_for('C'))
].

status_after_spec_validation(Spec) ->
	{SpecStatus, _} = alphadiamond_app:is_valid_spec(Spec),
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
