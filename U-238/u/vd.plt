:- begin_tests(vd).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/db_i)).
:- use_module(u(util/lambda)).

test(clear_test_db, [fail, setup(db_clear(people))]) :-

	db_recorded(people, _).

test(db_recorded_bug1, [setup(model_db), N =:= 3]) :-

   findall('.', db_recorded(people, _), L),
   length(L, N).

test(db_recorded, [setup(model_db)]) :-

   findall('.',
           (obj_construct(man_v, [sex], [man], X),
            db_recorded(people, X)),
           L1),
   length(L1, N1),
   assertion(N1 =:= 2),

   findall('.',
           (obj_construct(man_v, [sex], [man], X0),
            obj_rebase((object_v -> db_object_v), X0, X), 
            db_recorded(people, X)),
           L2),
   length(L2, N2),
   assertion(N2 =:= 2).
   

test(db_copy, [setup(model_db), N =:= 3]) :-

   db_copy(people, people2),
   findall('.', db_recorded(people2, _), L),
   length(L, N).

test(db_put_objects,
    [setup(db_clear(people)),
     DB_Size =:= 3]
    ) :-

	maplist(obj_construct(man_v, [name]),
		[['Valera'], ['Vika'], ['Yura']],
		Men),

	db_put_objects(people,
		       arg_reorder(member, [2,1], Men),
		       fail),

	db_size(people, DB_Size).

test(store_and_retrieve1,
    [setup(db_clear(people)),
     man(Sex, Name, Surname, Weight, Height) ==
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


model_db :-

	db_clear(people),
	db_clear(people2),
        
        obj_construct(man_v, 
                      [sex, name], 
                      [man, 'Adam'], Man1), 
        db_put_object(people, Man1, _),

        obj_construct(man_v, 
                      [sex, name], 
                      [woman, 'Eva'], Man2), 
        db_put_object(people, Man2, _),

        obj_construct(citizen_v, 
                      [sex, surname, country], 
                      [man, 'Mayakovsky', ['Soviet Union']],
                      Man3), 
        db_put_object(people, Man3, _). 


:- end_tests(vd).











