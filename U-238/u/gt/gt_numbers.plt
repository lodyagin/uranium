:- begin_tests(gt_numbers).
:- use_module(library(clpfd)).
:- use_module(u(gt/gt_numbers)).

test(known_seed, []) :-
    random_number([seed(2, Next_Seed), integer, range(0..10)], Num),
    assertion(Next_Seed == 2207042835),
    assertion(Num == 8).

:- end_tests(gt_numbers).
