:- use_module(ur_option).
:- use_module(u(v)).

:- begin_tests(ur_option, [setup(setup_options)]).

test(single_option_nonrep, [setup(setup_options)]) :-

   options_object(test_pred1, [width(-1), length(3)], Obj),
   assertion(Obj / length =^= length(3)),
   assertion(Obj / width =^= width(-1)),

test(single_option_unknown,
     [error(domain_error(valid_option, Option), _)]) :-

   Option = width(3),
   options_object(test_pred1, [Option], _).

test(single_option_no_default, [setup(setup_options)]) :-

   options_object(test_pred1, [], Obj),
   Obj / length ^= Length,
   assertion(Length =@= _).

test(single_option_default, [setup(setup_options)]) :-

   options_object(test_pred3, [], Obj),
   assertion(Obj / length =^= length(4)).

test(single_option_default_overriden, [setup(setup_options)]) :-

   options_object(test_pred3, [length(3)], Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep1,
     [error(domain_error(consistent_options, Options),
            _)]) :-

   Options = [length(3), length(_)],
   options_object(test_pred1, Options, Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep2,
     [error(domain_error(nonrepeating_options, Options),
            _)]) :-

   Options = [length(3), length(3)],
   options_object(test_pred1, Options, Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_rep3,
     [error(domain_error(consistent_options, Options),
            _)]) :-

   Options = [length(3), length(4)],
   options_object(test_pred1, Options, Obj),
   assertion(Obj / length =^= length(3)).

test(single_option_meta1) :-

   
   options_object(test_pred2, [generator(test_pred1)],
                  Obj),
   context_module(That),
   assertion(Obj / generator
            =^= generator(That:test_pred1)).

test(single_option_meta2) :-

   options_object(test_pred2, [generator(dummy:test_pred1)],
                  Obj),
   assertion(Obj / generator
            =^= generator(dummy:test_pred1)).

test(single_option_meta3) :-

   options_object(test_pred1, [length(_:test_pred1)],
                  Obj),
   assertion(Obj / length =^= length(_:test_pred1)).

test(single_option_bad_default1,
     [error(invalid_option_definition(Rule), _)]) :-

   Rule = [meta_option(length/1), default(len(3))],
   ur_options(test_pred4, [Rule]).

test(single_option_bad_default1,
     [error(invalid_option_definition(Rule), _)]) :-

   Rule = [option(length/1), default(length(3, 4))],
   ur_options(test_pred5, [Rule]).

test(single_option_repeated1, [fail]) :-

   ur_options(test_pred6, [[option(length/1)],
                           [option(width/1)],
                           [option(length/1)]]).

test(single_option_repeated2, [fail]) :-

   ur_options(test_pred7, [[option(length/1)],
                           [option(width/1)],
                           [option(length/3)]]).

setup_options :-

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   ur_options(test_pred1, [[option(length/1)], [option(width/1)]]),
   ur_options(test_pred2, [[meta_option(generator/1)]]),
   ur_options(test_pred3, [[option(length/1), default(length(4))]]).

test_pred1(_).
test_pred2(_).
test_pred3(_).
test_pred4(_).
test_pred5(_).
test_pred6(_).
test_pred7(_).

:- end_tests(ur_option).
