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

test(test2, fail) :-
   enum_size(nonexisting_module:_).

test(basic_enum_field) :-
  basic_enum_field(a, X),
  assertion(X == 'a#'),
  basic_enum_field(a, 'a#'),
  basic_enum_field('a#', Y),
  assertion(Y == 'a#'),
  basic_enum_field(Z, 'a#'),
  assertion(Z == a).

:- end_tests(ur_enums).
