:- begin_tests(ur_enums).
:- use_module(u(ur_enums)).
:- use_module(u(class_options)).

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

test(enum_integer_opts_strict1, 
     error(existence_error(enum, _, e98ddcdab52605ba69a5zz, _), _)
) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  enum_integer([], e98ddcdab52605ba69a5zz, _).

test(enum_integer_opts_strict2, 
     error(existence_error(enum, _, _, 2000000), _)
) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  enum_integer([], _, 2000000).

test(enum_integer_opts_unbound, I =@= _) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  enum_integer([weak], e98ddcdab52605ba69a5zz, I).

test(enum_integer_opts_fail, fail) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  enum_integer([fail], e98ddcdab52605ba69a5zz, _).

test(enum_integer_opts_global_scope_strict_policy, 
     error(enum_query_error(_), _)) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  assert_enum(edfdc2:[e98ddcdab52605ba69a54c, f64028b8f2dcecf2]),
  enum_integer([], e98ddcdab52605ba69a54c, _).

test(enum_integer_opts_global_scope_strict_policy1, 
     error(enum_query_error(_), _)) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  assert_enum(edfdc2:[e98ddcdab52605ba69a54c, f64028b8f2dcecf2]),
  enum_integer([], e98ddcdab52605ba69a54c, _).

test(enum_integer_opts_global_scope_strict_policy1, 
     error(enum_query_error(_), _)
) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  enum_integer([], _, 2).

test(enum_integer_opts_global_scope_weak_policy, Count == 2) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  assert_enum(edfdc2:[f64028b8f2dcecf2, e98ddcdab52605ba69a54c]),
  aggregate_all(count, 
                enum_integer([weak_scope], e98ddcdab52605ba69a54c, _),
                Count).

test(enum_integer_opts_module_scope, I == 1) :-
  assert_enum(edfdc1:[e98ddcdab52605ba69a54c, f64028b8f2dcecf0]),
  assert_enum(edfdc2:[f64028b8f2dcecf2, e98ddcdab52605ba69a54c]),
  enum_integer([module(edfdc2)], e98ddcdab52605ba69a54c, I).

:- end_tests(ur_enums).
