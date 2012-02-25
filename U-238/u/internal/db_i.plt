:- begin_tests(db_i).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/db_i)).

test(db_next_class_id, [setup(db_clear(db_i_test))]) :-

	db_i:db_next_class_id(db_i_test, Id1),
	assertion(Id1 =:= 2),
	db_i:db_next_class_id(db_i_test, Id2),
	assertion(Id2 =:= 3),
	db_i:db_next_class_id(db_i_test, Id3),
	assertion(Id3 =:= 4).

test(db_class_id1, [setup(db_clear(db_i_test))]) :-

	obj_construct(man_v, [], [], Man1),
	obj_rebase((object_v -> db_object_v), Man1, Man),
	arg(1, Man, Man_Local_Id),

	db_i:db_add_class(db_i_test, Man_Local_Id, Man_DB_Id),

	db_i:db_class_id(db_i_test, Man_Local_Id, Man_DB_Id1),
	assertion(Man_DB_Id =:= Man_DB_Id1),

	% object_v
	class_id(Object_Local_Id, object_v),
	db_i:db_class_id(db_i_test, Object_Local_Id, Object_DB_Id),
	assertion(Object_DB_Id =:= 1), % always 1
	assertion(Man_DB_Id =\= Object_DB_Id).



:- end_tests(db_i).
