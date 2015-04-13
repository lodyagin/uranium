:- module(citizen_v, []).

:- use_module(u(v)).

% TODO
% Class eval should be always coded with clpfd
% to allow reverse class -> functor calculation
% (e.g. in db_select/3).
'citizen_v?'(Term, class, callup_v) :-

        obj_field(Term, sex, Sex),
        memberchk(Sex, [man, male]),
        obj_field(Term, age, Age),
        integer(Age),
        between(18, 25, Age), !.

'citizen_v?'(Term, class, citizen_v) :-

        \+ 'citizen_v?'(Term, class, callup_v).

'citizen_v?'(Term, age, Age) :-

        get_time(TS),
        stamp_date_time(TS, DT, local),
        date_time_value(year, DT, Current_Year),
        
        obj_field(Term, birthday, Birthday),
        (  number(Birthday)
        -> Age is Current_Year - Birthday
        ;  number(Age)
        -> Birthday is Current_Year - Age
        ;  true % leave both Age and Birthday unbound
        ).
        

new_class(citizen_v, man_v, 
          [country, 
           id, 
           birthday : citizen_v_birthday_t
          ], 
          [id]).

typedef(citizen_v_birthday_t, [value_set - (gt_numbers:random_number)]).

setup_options :-
   Random = [[meta_option(generator/1)], 
             [group(rand_state), option(rand_state/1), option(rand_state/2)],
             [group(det), option(semidet/0), option(nondet/0)]
            ],
   ur_options(global:birthday, 
            [ [multi_group(domain), option(integer/0), default([integer])],
              [multi_group(pattern), option(range/1), default([range(1903..2015)])]
            | Random 
            ]),
   ur_options(global:citizen_v, Random).
