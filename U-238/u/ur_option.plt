:- use_module(ur_option).
:- use_module(library(assoc)).
:- use_module(u(v)).

:- begin_tests(ur_option, [setup(setup_options)]).

test(options_to_assoc1) :-
   options_to_assoc([], Type, Assoc),
   assertion(Type == list),
   assertion(Assoc == []).

test(options_to_assoc2) :-
   options_to_assoc([a], Type, Assoc),
   assertion(Type == list),
   assertion(Assoc == [a]).

test(options_to_assoc3) :-
   options_to_assoc([a, b(4)], Type, Assoc),
   assertion(Type == list),
   assertion(Assoc == [a, b(4)]).

test(options_to_assoc4) :-
   options_to_assoc([a-1], Type, Assoc),
   assertion(Type == assoc),
   get_assoc(a, Assoc, Value),
   assertion(Value == 1).

test(options_to_assoc5) :-
   options_to_assoc([a-1, b-3], Type, Assoc),
   assertion(Type == assoc),
   get_assoc(a, Assoc, Value1),
   get_assoc(b, Assoc, Value2),
   assertion(Value1 == 1),
   assertion(Value2 == 3).

test(options_to_assoc6) :-
   list_to_assoc([a-1, b-3], Opts),
   options_to_assoc(Opts, Type, Assoc),
   assertion(Type == assoc),
   get_assoc(a, Assoc, Value1),
   get_assoc(b, Assoc, Value2),
   assertion(Value1 == 1),
   assertion(Value2 == 3).

test(options_to_assoc7) :-
   options_to_assoc(mod1:[a-1, b-3], Type, Mod2:Assoc),
   assertion(Type == assoc),
   get_assoc(a, Assoc, Value1),
   get_assoc(b, Assoc, Value2),
   assertion(Value1 == 1),
   assertion(Value2 == 3),
   assertion(Mod2 == mod1).

test(filter_assoc1, [Opts == []]) :-
   filter_assoc([], Opts).

test(filter_assoc2, [Opts == [a]]) :-
   filter_assoc([a], Opts).

test(filter_assoc3, [Opts == [a, b(4)]]) :-
   filter_assoc([a, b(4)], Opts).

test(filter_assoc4, [Opts == []]) :-
   filter_assoc([a-1], Opts).

test(filter_assoc5, [Opts == []]) :-
   filter_assoc([a-1, b-3], Opts).

test(filter_assoc6, [Opts == []]) :-
   list_to_assoc([a-1, b-3], A),
   filter_assoc(A, Opts).

test(filter_assoc7) :-
   filter_assoc(mod1:[o1, o2], Mod2:Opts),
   assertion(Opts == [o1, o2]),
   assertion(Mod2 == mod1).

test(filter_assoc8) :-
   filter_assoc(mod1:[o1-1, o2-2], Mod2:Opts),
   assertion(Opts == []),
   assertion(Mod2 == mod1).

test(filter_assoc9) :-
   list_to_assoc([a-1, b-3], A),
   filter_assoc(mod1:A, Mod2:Opts),
   assertion(Opts == []),
   assertion(Mod2 == mod1).


test(single_option_nonrep, [setup(setup_options)]) :-

   options_object(test_pred1, [height(-1), length(3)], Obj),
   assertion(Obj / length =^= length(3)),
   assertion(Obj / height =^= height(-1)).

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

test(single_option_repeated1,
     [error(invalid_option_definition(Options), _)]
    ) :-

   Options = [[option(length/1)],
              [option(width/1)],
              [option(length/1)]],
   ur_options(test_pred6, Options).

test(single_option_repeated2) :-

   ur_options(test_pred7, [[option(length/1)],
                           [option(width/1)],
                           [option(length/3)]]).

test(group_option_repeated1,
     [error(invalid_option_definition(O), _)]) :-

   O = [[group(length),
         option(length/1), option(length/3)],
        [option(width/1)],
        [option(length/3)]],
   ur_options(test_pred8, O).

test(group_option1) :-
   options_object(test_pred9,
                  [empty, generator(test_pred1)],
                  Obj),
   context_module(That),
   assertion(Obj / length =^= empty),
   assertion(Obj/generator=^=generator(That:test_pred1)).

test(group_option2,
    [error(domain_error(one_option_per_group, _), _)]) :-
   
   options_object(test_pred9,
                  [empty, generator(test_pred1),
                   length(1)], _).

test(group_option3) :-
   options_object(test_pred9, [], Obj),
   assertion(Obj / length =^= _).
   
test(multi_group_option1) :-

   OL = [empty, length(5), length(5), length(9)],
   options_object(test_pred10, OL, Obj),
   Obj / length ^= OL2,
   msort(OL2, OL3),
   msort(OL, OL4),
   assertion(OL3 == OL4).

test(multi_group_option2,
     [error(invalid_option_definition(O), _)]) :-

   O = [multi_group(length),
        option(empty/0), option(length/1),
        option(length/1)],
   ur_options(test_pred11, [O]).

test(multi_group_option3,
     [error(invalid_option_definition(O), _)]) :-

   O = [multi_group(length),
        option(empty/0), option(length/1),
        meta_option(length/1)],
   ur_options(test_pred12, [O]).

test(multi_group_option4,
     [error(invalid_option_definition(O), _)]) :-

   O = [multi_group(length),
        option(empty/0), option(length/1),
        default(empty/0)],
   ur_options(test_pred13, [O]).

test(multi_group_option5,
     [error(invalid_option_definition(O), _)]) :-

   O = [multi_group(length),
        option(empty/0), option(length/1),
        default([length/1, width/2])],
   ur_options(test_pred14, [O]).

setup_options :-

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   ur_options(test_pred1,
              [[option(length/1)], [option(height/1)]]),
   ur_options(test_pred2,
              [[meta_option(generator/1)]]),
   ur_options(test_pred3,
              [[option(length/1), default(length(4))]]),
   ur_options(test_pred9,
              [[group(length), option(empty/0),
                option(length/1), option(length/2),
                meta_option(length_pred/1)],
               [meta_option(generator/1)]]),
   ur_options(test_pred10,
              [[multi_group(length), option(empty/0),
                option(length/1), option(length/2),
                default([empty, length(1, 80)]),
                meta_option(length_pred/1)],
               [meta_option(generator/1)]]).

test_pred1(_).
test_pred2(_).
test_pred3(_).
test_pred4(_).
test_pred5(_).
test_pred6(_).
test_pred7(_).
test_pred8(_).
test_pred9(_).
test_pred10(_).
test_pred11(_).
test_pred12(_).
test_pred13(_).
test_pred14(_).

:- end_tests(ur_option).
