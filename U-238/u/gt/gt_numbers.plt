:- begin_tests(gt_numbers).
:- use_module(library(clpfd)).
:- use_module(u(gt/gt_numbers)).
:- use_module(u(v)).

test(known_seed, []) :-
    random_number([seed(2, Next_Seed), integer, range(0..10)], OptsOut, Num),
    assertion(Next_Seed == 2207042835),
    assertion(Num == 8),
    OptsOut / global_options / rand_options / seed ^= seed(S1, S2),
    assertion(S1 == Next_Seed),
    assertion(S2 =@= _).

test(random_number1_nondet, [L == [p(2, 1), p(4, 2), p(3, 3), p(1, 3)]]) :-
    findall(p(X, S), 
            random_number([generator(randgen:test_sequence1), seed(0, S), 
                           nondet, range(1..4)], _, X), 
            L).

:- end_tests(gt_numbers).
