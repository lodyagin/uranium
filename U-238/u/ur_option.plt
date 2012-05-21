:- begin_tests(ur_option).
:- use_module(ur_option).
:- use_module(u(v)).

test(single_option_nonrep) :-

   ur_options(some_test_pred1, [[option(length/1)]]),
   options_object(some_test_pred1, [length(3)], Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep1,
     [error(domain_error(consistent_options, Options),
            _)]) :-

   Options = [length(3), length(_)],
   ur_options(some_test_pred2, [[option(length/1)]]),
   options_object(some_test_pred2, Options, Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep2,
     [error(domain_error(nonrepeating_options, Options),
            _)]) :-

   Options = [length(3), length(3)],
   ur_options(some_test_pred2, [[option(length/1)]]),
   options_object(some_test_pred2, Options, Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep3,
     [error(domain_error(consistent_options, Options),
            _)]) :-

   Options = [length(3), length(4)],
   ur_options(some_test_pred2, [[option(length/1)]]),
   options_object(some_test_pred2, Options, Obj),
   assertion(Obj / length =^= length(3)).

some_test_pred1(_).
some_test_pred2(_).

:- end_tests(ur_option).
