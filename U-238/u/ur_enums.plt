:- begin_tests(ur_enums).
:- use_module(u(ur_enums)).

test(test1) :-
   assert_enum([red, orange, yellow, green, blue, indigo, violet]),
   enum_size(Size),
   assertion(Size == 7),
   enum_integer(orange, OrangeInt),
   assertion(OrangeInt == 1),
   enum_integer(Colour, 4),
   assertion(Colour == blue), !.

:- end_tests(ur_enums).
