:- begin_tests(db_i).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/db_vocab)).

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

	% object_v
	class_id(Object_Local_Id, object_v), !,
	db_conv_local_db(db_i_test, Object_Local_Id,
			 Object_DB_Id, _),

	assertion(Object_DB_Id =:= 1), % always 1
	assertion(Man_DB_Id =\= Object_DB_Id).


test(db_recorded_int,
     [setup(setup),
      throws(error(no_object_field(Obj0, db_ref), _))]
    ) :-

        obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
        db_put_object(db_i_test, Obj0, _),
        db_recorded_int(db_i_test, Obj0).

test(db_recorded_int, [setup(setup)]) :-

        obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
        db_put_object(db_i_test, Obj0, Obj),
        db_recorded(db_i_test, Obj).

test(key_conflict, [setup(model_db(Slod1))]) :-

   obj_construct(man_v,
                 [name, surname], ['Sergei', 'Lodyagin'], Man1_0),
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
           List2),
   assertion(List2 =@= ['Lodyagin', 'Sikorsky'
                        %'Sysoev', 'Ivan'
                        % (man_v is not desc. of citizen_v)
                        ]),

   obj_construct(man_v, [], [], Man3_0),
   obj_rebase((object_v -> db_object_v), Man3_0, Man3),

   findall(C,
           key_conflict(people, Man_V_Re_Id, Man3, C),
           List3),
   length(List3, N3),
   assertion(N3 =:= 5),

   obj_construct(citizen_v, [name, id], ['Sergei', 4], Man4_0),
   obj_rebase((object_v -> db_object_v), Man4_0, Man4),

   arg(1, Man4, Citizen_V_Re_Id),
   
   findall(Name,
           (  key_conflict(people, Citizen_V_Re_Id, Man4, O),
              obj_field(O, surname, Name)
           ),
           List4),
   assertion(List4 =@= ['Lodyagin', 'Sikorsky', 'Sysoev']),
   
   obj_construct(citizen_v,
                 [name, surname, id],
                 ['Igor', 'Litvin', 4], Man5_0),
   obj_rebase((object_v -> db_object_v), Man5_0, Man5),

   findall(Name,
           (  key_conflict(people, Citizen_V_Re_Id, Man5, O),
              obj_field(O, surname, Name)
           ),
           List5),
   assertion(List5 =@= ['Sysoev']).
   
   

model_db(Slod1) :-

   db_clear(people),
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin'], Slod1),
   
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Sikorsky']),
   db_construct(people, man_v, [name], ['Ivan']),
   db_construct(people, man_v,
                [name, surname], ['Artemiy', 'Lodyagin']),
   db_construct(people, man_v,
                [name, surname], ['Artemiy', 'Lebedev']),
   db_construct(people, citizen_v,
                [name, surname, id], ['Sergei', 'Sysoev', 4]),
   db_construct(people, citizen_v,
                [name, surname, id], ['Sergei', 'Ivan', 5]).

setup :-

	db_clear(db_i_test).

:- end_tests(db_i).
