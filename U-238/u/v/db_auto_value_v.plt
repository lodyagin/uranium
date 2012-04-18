:- begin_tests(db_auto_value_v).
:- use_module(u(v/db_auto_value_v)).
:- use_module(u(util/lambda)).
:- use_module(u(v)).
:- use_module(u(vd)).

test(new_db_auto_value1,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))
      ]) :-

   new_db_auto_value(people, abc_v, id1, succ, 1),
   new_db_auto_value(people, abc_v, id1, succ, 2).

test(new_db_auto_value1,
     [setup((db_clear(people), db_clear(people2)))]) :-

   new_db_auto_value(people, abc_v, id1, succ, 1),
   new_db_auto_value(people, abc_v, id2, succ, 2),
   new_db_auto_value(people, abcd_v, id1, succ, 3),
   new_db_auto_value(people2, abc_v, id1, succ, 4).

test(db_auto_value1,
     [setup(db_clear(people)), L == [4, 5, 6]]) :-

   new_db_auto_value(people, citizen_v, id, succ, 3),
   length(L, 3),
   maplist(db_auto_value(people, citizen_v, id), L).

test(db_auto_value2, [setup(people_db)]) :-

   bagof(p(Class, Field, Value),
         Class^Field^db_auto_value(people, Class, Field, Value),
         L1),
   assertion(L1 == [p(man_v, name, acb),
                    p(citizen_v, id, 1),
                    p(callup_v, id, 101)]),

   bagof(p(Class, Field, Value),
         Class^Field^db_auto_value(people, Class, Field, Value),
         L2),
   assertion(L2 == [p(man_v, name, bac),
                    p(citizen_v, id, 2),
                    p(callup_v, id, 102)]).
   

test(db_auto_value3, [setup(people_db), Name == abc]) :-

   db_auto_value(people, citizen_v, name, Name).


test(db_auto_value4, [setup(people_db), fail]) :-

   db_auto_value(people, man_v, id, _).
   

test(db_bind_auto,
     [setup(people_db),
      LOL =@= [[man_v, _, acb],
               [citizen_v, 1, bac],
               [callup_v, 101, bca]
               ]
     ]) :-

   obj_construct(man_v, [], [], Man),
   obj_construct(citizen_v, [], [], Citizen),
   obj_construct(callup_v, [], [], Callup),

   L = [Man, Citizen, Callup],

   forall(member(Obj, L),
          (  db_bind_auto(people, Obj),
             db_put_object(people, Obj)
          )),

   db_select_list(people, _, [functor, id, name], LOL).
   

people_db :-

   db_clear(people),

   new_db_auto_value(people, man_v, name,
                     \Old^New^(atom_codes(Old, Old_Codes),
                               (permutation(Old_Codes, C2),
                               atom_codes(New, New_Codes)
                              ),
                     p(abc, 2)),
   new_db_auto_value(people, citizen_v, id, succ, 0),
   new_db_auto_value(people, callup_v, id, succ, 100).
   
:- end_tests(db_auto_value_v).

