:- module(man_v, []).

:- use_module(u(ur_option)).
:- use_module(u(v)).

new_class(man_v, object_v, 
          [sex : man_v_sex_t, name, surname, weight, height]
         , [name, surname]
         ).

new_class(passport_v, object_v, [sex, name, surname]).

% any man with 3 fields defined can be reinterpreted as a passport ...
reinterpret(man_v, passport_v, From, _) :-
  obj_unify(From, [sex, name, surname], [Sex, Name, Surname]),
  ground(Sex), ground(Name), ground(Surname).

typedef(man_v_sex_t, [value_set - (object_v:list_member_gen(sex))]).

setup_options :-
   Random = [[meta_option(generator/1)], 
             [group(seed), option(seed/1), option(seed/2)],
             [group(det), option(semidet/0), option(nondet/0)]
            ],
   ur_options(global:sex, %TODO add Class par to list_member_gen
              [[multi_group(list),
               option(male/0),
               option(female/0),
               option(unknown/0),
               default([unknown])]
              | Random
              ]),
   ur_options(global:man_v, Random).


