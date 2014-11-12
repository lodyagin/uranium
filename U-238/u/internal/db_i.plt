:- begin_tests(db_i).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/db_vocab)).

test(db_functor_des, [setup(model_db(_))]) :-

   findall(F, db_functor_des(people, F, _, _), List1_0),
   msort(List1_0, List1),
   assertion(List1 == [citizen_v, db_object_v, man_v]),

   findall(F,
           (F = man_v, db_functor_des(people, F, _, _)),
           List2_0),
   msort(List2_0, List2),
   assertion(List2 == [man_v]),

   findall(F,
           (F = citizen_v, db_functor_des(people, F, _, _)),
           List3_0),
   msort(List3_0, List3),
   assertion(List3 == [citizen_v]),

   findall(F,
           (F = callup_v, db_functor_des(people, F, _, _)),
           List4),
   assertion(List4 == []).


test(db_next_class_id, [setup(setup)]) :-

	db_i:db_next_class_id(db_i_test, Id1),
	assertion(Id1 =:= 2),
	db_i:db_next_class_id(db_i_test, Id2),
	assertion(Id2 =:= 3),
	db_i:db_next_class_id(db_i_test, Id3),
	assertion(Id3 =:= 4).

% test local class id -> db class id conversion
test(db_conv_local_db1, [setup(setup)]) :-
	obj_construct(man_v, [], [], Man1),
	obj_rebase((object_v -> db_object_v), Man1, Man),
	arg(1, Man, Man_Local_Id),
	db_i:db_add_class(db_i_test, Man_Local_Id, Man_DB_Id, _),
	db_conv_local_db(db_i_test, Man_Local_Id,
			 Man_DB_Id1,_),
	assertion(Man_DB_Id =:= Man_DB_Id1),
        assertion(Man_DB_Id =\= 1).

test(db_conv_local_db2, [setup(setup), Object_DB_Id == 1]) :-
        % object_v
	class_id(Object_Local_Id, object_v), !,
	db_conv_local_db(db_i_test, Object_Local_Id,
			 Object_DB_Id, _).

test(db_conv_local_db3, [setup(setup)]) :-
        % not db_object_v descendant
	class_primary_id(man_v, Object_Local_Id), !,
	db_conv_local_db(db_i_test, Object_Local_Id,
			 _, _).

test(db_key_policy, [setup(setup)]) :-

   db_key_policy(db_i_test, Old1, New1),
   assertion(Old1 == throw),
   assertion(New1 == throw),

   db_key_policy(db_i_test, Old2, overwrite),
   assertion(Old2 == throw),

   db_key_policy(db_i_test, _, overwrite),
   db_key_policy(db_i_test, overwrite, _),
   db_key_policy(db_i_test, overwrite, overwrite),

   db_key_policy(db_i_test, overwrite, throw),

   db_key_policy(db_i_test, Old3, New3),
   assertion(Old3 == throw),
   assertion(New3 == throw).   
   

test(db_recorded_int1,
     [setup(setup),
      error(domain_error(db_object_v_descendant, Obj0))]
    ) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
   db_put_object(db_i_test, Obj0, _),
   db_recorded_int(db_i_test, Obj0).

test(db_recorded_int2, [setup(setup)]) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
   db_put_object(db_i_test, Obj0, Obj),
   db_recorded(db_i_test, Obj).

test(db_recorded_int3, [setup(setup)]) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
   obj_rebase((object_v -> db_object_v), Obj0, Obj1),
   db_put_object(db_i_test, Obj1, Obj),
   db_recorded(db_i_test, Obj).

test(db_recorded_int4, [setup(setup)]) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
   obj_rebase((object_v -> db_object_v), Obj0, Obj1),
   obj_rebase((object_v -> tarjan_vertex_v), Obj1, Obj2),
   db_put_object(db_i_test, Obj2, Obj),
   db_recorded(db_i_test, Obj),
   db_clear(db_i_test).

test(key_conflict1, [setup(model_db(Slod1))]) :-

   obj_construct(man_v,
                 [name, surname], ['Sergei', 'Lodyagin'],
                 Man1_0),
   obj_rebase((object_v -> db_object_v), Man1_0, Man1),

   arg(1, Man1, Man_V_Re_Id),

   findall(C,
           key_conflict(people, Man_V_Re_Id, Man1, C),
           List1),
   assertion(List1 =@= [Slod1]),

   obj_construct(man_v, [name], ['Sergei'], Man2_0),
   obj_rebase((object_v -> db_object_v), Man2_0, Man2),

   findall(Name,
           (  key_conflict(people, Man_V_Re_Id, Man2, O),
              obj_field(O, surname, Name)
           ),
           List2_U),
   msort(List2_U, List2),
   assertion(List2 =@= ['Ivan', 'Lodyagin', 'Sikorsky',
                        'Sysoev']),

   obj_construct(man_v, [], [], Man3_0),
   obj_rebase((object_v -> db_object_v), Man3_0, Man3),

   findall(C,
           key_conflict(people, Man_V_Re_Id, Man3, C),
           List3),
   length(List3, N3),
   assertion(N3 =:= 8),

   obj_construct(citizen_v, [name, id], ['Sergei', 4],
                 Man4_0),
   obj_rebase((object_v -> db_object_v), Man4_0, Man4),

   arg(1, Man4, Citizen_V_Re_Id),
   
   findall(Name,
           (key_conflict(people, Citizen_V_Re_Id, Man4, O),
            obj_field(O, surname, Name)
           ),
           List4_U),
   sort(List4_U, List4),
   assertion(List4 =@= ['Lodyagin', 'Sikorsky', 'Sysoev']),
   
   obj_construct(citizen_v,
                 [name, surname, id],
                 ['Igor', 'Litvin', 4], Man5_0),
   obj_rebase((object_v -> db_object_v), Man5_0, Man5),

   findall(Name,
           (key_conflict(people, Citizen_V_Re_Id, Man5, O),
            obj_field(O, surname, Name)
           ),
           List5),
   assertion(List5 =@= ['Sysoev']),

   obj_construct(callup_v,
                 [name, surname, id],
                 ['Igor', 'Litvin', 4], Man6_0),
   obj_rebase((object_v -> db_object_v), Man6_0, Man6),
   arg(1, Man6, Callup_V_Re_Id),

   findall(Name,
           ( key_conflict(people, Callup_V_Re_Id, Man6, O),
             obj_field(O, surname, Name)
           ),
           List6),
   assertion(List6 =@= ['Sysoev']).

test(key_conflict2, [setup(model_db2)]) :-

   obj_construct(citizen_v,
                 [name, surname, id],
                 ['Igor', 'Litvin', 4], Man5_0),
   obj_rebase((object_v -> db_object_v), Man5_0, Man5),
   arg(1, Man5, Citizen_V_Re_Id),
   
   findall(Name,
          (key_conflict(people2, Citizen_V_Re_Id, Man5, O),
            obj_field(O, surname, Name)
           ),
           List5),
   assertion(List5 =@= ['Petrenko']).
   

model_db(Slod1) :-

   db_clear(people),
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin'],
                Slod1),
   
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Sikorsky']),
   db_construct(people, man_v, [name], ['Ivan']),
   db_construct(people, man_v,
                [name, surname], ['Artemiy', 'Lodyagin']),
   db_construct(people, man_v,
                [name, surname], ['Artemiy', 'Lebedev']),
   db_construct(people, citizen_v,
                [name, surname, id],
                ['Sergei', 'Sysoev', 4]),
   db_construct(people, citizen_v,
                [name, surname, id],
                ['Sergei', 'Ivan', 5]),
   db_construct(people, citizen_v,
                [name, surname, id],
                ['Igor', 'Ivan', 6]).

model_db2 :-

   db_clear(people2),
   db_construct(people2, callup_v,
                [id, surname], [4, 'Petrenko']).

setup :-

	db_clear(db_i_test).

:- end_tests(db_i).
