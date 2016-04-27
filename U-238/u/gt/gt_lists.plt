:- begin_tests(gt_lists).
:- use_module(u(gt/gt_lists)).

test(random_sublist_smoketest, N == 32) :-
   aggregate_all(count,
                 random_sublist([1, 2, 3, 4, 5],
                                [nondet, length(0..5)], _, _),
                 N).

:- end_tests(gt_lists).