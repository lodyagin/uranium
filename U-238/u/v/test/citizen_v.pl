:- module(citizen_v, []).

'citizen_v?'(Term, class, callup_v) :-

        obj_field(Term, sex, Sex),
        Sex == man,
        obj_field(Term, age, Age),
        between(18, 25, Age), !.

'citizen_v?'(_, class, citizen_v).

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
        

new_class(citizen_v, man_v, [country, id, birthday]).
