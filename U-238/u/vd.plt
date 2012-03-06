:- begin_tests(vd).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/db_i)).
:- use_module(u(util/lambda)).

test(clear_test_db, [fail, setup(db_clear(people))]) :-

	db_recorded(people, _).

test(db_erase, [setup(model_db)]) :-

   (  named_args_unify(people, _,
                       [name, surname], ['Adam', _], X),
      db_erase(X),
      fail ; true
   ),

   findall(Y,
           db_recorded(people, Y),
           LY),
   assertion(LY = [man_v(_, _, _, 'Eva', woman, _, _)]).

test(db_iterate1, [setup(model_db), N =:= 3]) :-

   findall('.', db_iterate(people, true, _), L),
   length(L, N).

test(db_iterate2, [setup(model_db), N =:= 2]) :-

   findall(O, db_iterate(people, sex(man), O), L),
   writeln(L),
   length(L, N).

test(db_recorded_bug1, [setup(model_db), N =:= 3]) :-

   findall('.', db_recorded(people, _), L),
   length(L, N).

test(db_recorded1,
     [setup(model_db),
      throws(error(no_object_field(X, db_ref), _))]) :-

   obj_construct(man_v, [sex], [man], X),
   db_recorded(people, X).


test(db_recorded2, [setup(model_db)]) :-

   findall('.',
           (obj_construct(man_v, [sex], [man], X0),
            obj_rebase((object_v -> db_object_v), X0, X), 
            db_recorded(people, X)),
           L2),
   length(L2, N2),
   assertion(N2 =:= 1).
   

test(db_recorded3, [setup(model_db)]) :-

   findall('.',
           (obj_construct(man_v, [], [], X0),
            obj_rebase((object_v -> db_object_v), X0, X), 
            db_recorded(people, X)),
           L2),
   length(L2, N2),
   assertion(N2 =:= 2).
   
test(db_recorded4, [setup(model_db)]) :-

   findall('.',
           (obj_construct(man_v, [height], [1.76], X0),
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

test(named_args_unify1, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [sex], [man], _),
           ['.', '.']).

test(named_args_unify2, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [sex, sex], [man, man], _),
           ['.', '.']).

test(named_args_unify_bug1, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, citizen_v, [sex], [man], _),
           ['.']).

test(named_args_unify_bug2, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [], [], _),
           ['.', '.', '.']).

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











