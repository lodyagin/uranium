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
     [throws(error(domain_error(db_object_v_desc, _), _))]
    ) :-

        obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
        db_put_object(test2, Obj0, _),
        db_recorded_int(test2, Obj0).

test(db_recorded_int,
     [fail]) :-

        obj_construct(man_v, [sex, name], [man, 'Adam'], Obj0),
        db_put_object(test2, Obj0, Obj),
        db_recorded(test2, Obj).
     

setup :-

	db_clear(db_i_test).

:- end_tests(db_i).
