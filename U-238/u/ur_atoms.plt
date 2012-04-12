:- begin_tests(ur_atoms).
:- use_module(u(ur_atoms)).

test(capitalize_atom1, [X = '']) :-

   capitalize_atom('', X).

test(capitalize_atom2, [X = 'A']) :-

   capitalize_atom(a, X).

test(capitalize_atom3, [X = '4']) :-

   capitalize_atom('4', X).

test(capitalize_atom4, [X = 'Abc']) :-

   capitalize_atom(abc, X).

test(capitalize_atom5, [X = 'ABC']) :-

   capitalize_atom('ABC', X).

:- end_tests(ur_atoms).