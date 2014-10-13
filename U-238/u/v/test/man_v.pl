:- module(man_v, []).

new_class(man_v, object_v, [sex, name, surname, weight, height]
         , [name, surname]
         ).

new_class(passport_v, object_v, [sex, name, surname]).

% any man with 3 fields defined can be reinterpreted as a passport ...
reinterpret(man_v, passport_v, From, _) :-
  obj_unify(From, [sex, name, surname], [Sex, Name, Surname]),
  ground(Sex), ground(Name), ground(Surname).