:- begin_tests(vd,
	      [setup(prepare_db)]
	      ).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/db_i)).

test(clear_db, [fail]) :-

	db_recorded(people, _).

test(store_and_retrieve1,
    [man(Sex, Name, Surname, Weight, Height) ==
    man(man, 'Adam', 'Adamov', 1, 3)]
    ) :-

	obj_construct(man_v,
		      [sex, name, surname, weight, height],
		      [man, 'Adam', 'Adamov', 1, 3],
		      Man),

	db_put_object(people, Man, Man1),

	assertion(obj_is_descendant(Man1, db_object_v)),
	assertion(\+ obj_is_descendant(Man1, man_v)),
	assertion(\+ obj_is_descendant(Man, db_object_v)),

	obj_field(Man1, sex, Sex),
	obj_field(Man1, name, Name),
	obj_field(Man1, surname, Surname),
	obj_field(Man1, weight, Weight),
	obj_field(Man1, height, Height).

prepare_db :-

	db_clear(people).

:- end_tests(vd).

