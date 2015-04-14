:- begin_tests(gt_objects, [setup(setup_options)]).

:- use_module(u(gt/gt_objects)).
:- use_module(u(v)).
:- use_module(u(ur_option)).

test(obj_fill_random1_semidet) :-
    obj_construct(man_v, [], [], M),
    options_object(randgen:random_options, [sex-[male, female]], Opts),
    obj_construct(global_options_v, [rand_options], [[rand_state(2)]], GO),
    Opts / global_options ^= GO,
    obj_fill_random(Opts, _, M),
    assertion(M / sex =^= male).

test(obj_fill_random1_nondet) :-
    obj_construct(man_v, [], [], M),
    options_object(randgen:random_options, 
                   [sex-[male, female, nondet]], Opts),
    obj_construct(global_options_v, [rand_options], [[rand_state(2)]], GO),
    Opts / global_options ^= GO,
    findall(Sex, (obj_fill_random(Opts, _, M), M / sex ^= Sex), SexList),
    assertion(SexList == [male, female]).

test(obj_fill_dowcast_random1_nondet,
     FL == [citizen_v,citizen_v,citizen_v,citizen_v,
            citizen_v,citizen_v,citizen_v,citizen_v,
            citizen_v,callup_v,callup_v,citizen_v,
            citizen_v,callup_v,callup_v,citizen_v,
            citizen_v,citizen_v,citizen_v,citizen_v,
            citizen_v,citizen_v,callup_v,citizen_v,
            citizen_v,callup_v,citizen_v,citizen_v,
            citizen_v,callup_v,citizen_v,citizen_v,
            citizen_v,citizen_v,citizen_v,citizen_v,
            citizen_v,citizen_v,callup_v,citizen_v,
            citizen_v,citizen_v ]
    ) :-
    options_object(randgen:random_options,
                   [sex-[male, female, nondet],
                    birthday-[nondet, range(1980..2000)]
                   ],
                   Opts),
    obj_construct(global_options_v,
                  [rand_options],
                  [[rand_state(2), generator(randgen:test_sequence1)]],
                  GO),
    Opts / global_options ^= GO,
    findall(F,
            ( obj_construct(citizen_v, [], [], M),
              obj_fill_downcast_random(Opts, _, M, M1),
              functor(M1, F, _)
            ),
            FL).

% This test differs from the previous one because now a seed is transmitted
% from random function to random function in sequence.
test(obj_fill_dowcast_random_list1_nondet,
     FL == [ callup_v,citizen_v,citizen_v,citizen_v,
             citizen_v,citizen_v,callup_v,citizen_v,
             citizen_v,citizen_v,citizen_v,citizen_v,
             citizen_v,citizen_v,callup_v,callup_v ]
    ) :-
    options_object(randgen:random_options,
                   [sex-[male, female, nondet],
                    birthday-[nondet, range(1980..2000)]
                   ],
                   Opts),
    obj_construct(global_options_v,
                  [rand_options],
                  [[rand_state(pcg32_init(42,52)), 
                    generator(randgen:pcg32_1)]],
                  GO),
    Opts / global_options ^= GO,
    length(L, 16),
    bagof(M, L^(member(M, L), obj_construct(citizen_v, [], [], M)), LM0),
    obj_fill_downcast_random_list(Opts, _, LM0, LM), !,
    findall(F, (member(M, LM), functor(M, F, _)), FL).


setup_options :-
    man_v:setup_options,
    citizen_v:setup_options.

:- end_tests(gt_objects).
