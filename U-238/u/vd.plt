:- begin_tests(vd).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/db_i)).
:- use_module(u(util/lambda)).

test(clear_test_db, [fail, setup(db_clear(people))]) :-

	db_recorded(people, _).

test(db_copy, [setup(model_db), N =:= 3]) :-

   db_copy(people, people2),
   findall('.', db_recorded(people2, _), L),
   length(L, N).

test(db_erase, [setup(model_db)]) :-

   (  named_args_unify(people, _,
                       [weight, surname], [73, _], X),
      db_erase(X),
      fail ; true
   ),

   findall(Y,
           db_recorded(people, Y),
           LY),
   assertion(LY = [man_v(_, _, _, _, 'Eva', woman, _, _)]).

test(db_iterate1, [setup(model_db), N =:= 3]) :-

   findall('.', db_iterate(people, true, _), L),
   length(L, N).

test(db_iterate2, [setup(model_db), N =:= 2]) :-

   findall(O, db_iterate(people, sex(man), O), L),
   length(L, N).

test(db_iterate3, [setup(model_db), fail]) :-

   db_iterate(people, unexisting_name(man_v), _).

test(db_iterate4, [setup(model_db), N =:= 2]) :-

   findall(O, db_iterate(people, functor(man_v), O), L),
   length(L, N).

test(db_iterate4, [setup(model_db), N =:= 3]) :-

   findall(O, db_iterate(people, functor(_), O), L),
   length(L, N).

test(db_to_list, [setup(model_db)]) :-

   db_to_list(people, man_v, L1),
   length(L1, N1),
   assertion(N1 =:= 2),

   db_to_list(people, citizen_v, L2),
   length(L2, N2),
   assertion(N2 =:= 1),

   db_to_list(people, _, L3),
   length(L3, N3),
   assertion(N3 =:= 3).

test(db_put_object1, [setup(model_db)]) :-

   obj_construct(man_v, [name], ['Moses'], Man),
   db_put_object(people, Man).

test(db_put_object2, [setup(model_db)]) :-

   obj_construct(man_v, [name], ['Moses'], Man0),
   obj_rebase((object_v -> db_object_v), Man0, Man),
   db_put_object(people, Man).

test(db_put_object3,
     [setup(model_db),
      error(db_obj_replace_protector(people2, Man))]) :-

   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man), !,
   db_put_object(people2, Man).

test(db_put_object4, [setup(model_db)]) :-

   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man0), !,
   obj_rewrite(Man0, [name], ['Moses'], ['Yorik'], Man1),
   db_put_object(people2, throw, Man1, Man2, Replaced),
   named_args_unify(Man2,
                    [name, db_key, db_ref],
                    [New_Name, DB_Key, DB_Ref]),
   assertion(New_Name == 'Yorik'),
   assertion(Replaced == replaced),
   assertion(nonvar(DB_Ref)),
   assertion(DB_Key == people2).

test(db_put_objects,
    [setup(db_clear(people)),
     DB_Size =:= 3]
    ) :-

	maplist(obj_construct(man_v, [name]),
		[['Valera'], ['Vika'], ['Yura']],
		Men),

	db_put_objects(people,
		       Men+\El^member(El, Men),
		       fail),

	db_size(people, DB_Size).


test(db_recorded_bug1, [setup(model_db), N =:= 3]) :-

   findall('.', db_recorded(people, _), L),
   length(L, N).

test(db_recorded1,
     [setup(model_db),
      error(domain_error(db_object_v_descendant, X))]) :-

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
   
test(db_recorded5, [setup(db_clear(people))]) :-

   db_construct(people, man_v, [], []),
   db_recorded(people, X), !,
   db_recorded(people, X).

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

test(key_rule1,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']).
                
test(key_rule2,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v, [], []),
   db_construct(people, man_v, [], []).
                
test(key_rule3,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v, [name], ['Sergei']),
   db_construct(people, man_v, [], []).
                
test(key_rule4,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v, [], []),
   db_construct(people, man_v, [name], ['Sergei']).
                
test(key_rule5, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   
   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Sikorsky']).

test(key_rule6,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name], ['Sergei']),
   
   db_construct(people, man_v,
                [name], ['Sergei']).

test(key_rule7,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name], ['Sergei']),
   
   db_construct(people, man_v,
                [surname], ['Lodyagin']).

test(key_rule8, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name], ['Sergei']),
   
   db_construct(people, man_v,
                [name], ['Artem']).

test(key_rule9,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Sysoev']),
   
   db_construct(people, man_v,
                [name], ['Sergei']).

test(key_rule10,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name], ['Sergei']),

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Sysoev']).
   
test(key_rule11,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, citizen_v, [], []).
                
test(key_rule12, [setup(db_clear(people))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [], []).
                
test(key_rule13, [setup(db_clear(people))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [name], ['James']).
                
test(key_rule14,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [], []),
   db_construct(people, citizen_v, [id], [5]).
                
test(key_rule15, [setup(db_clear(people))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v,
                [name, surname], ['James', 'Crick']),
   db_construct(people, citizen_v,
                [name, surname, id], ['Adam', 'Adamovitch', 5]).
   
test(key_rule16,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v, [name], ['James']),
   db_construct(people, citizen_v, [id], [4]).


test(db_singleton_v1, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   db_construct(people, db_singleton_v,
                [key_policy], [overwrite]),
   db_construct(people, man_v, [surname], ['Lodyagin']),
   
   true.

                
   
model_db :-

	db_clear(people),
	db_clear(people2),
        
        db_construct(people, man_v,
                     [sex, name, weight], [man, 'Adam', 73]), 
        db_construct(people, man_v,
                     [sex, name, weight], [woman, 'Eva', 64]), 

        db_construct(people, citizen_v, 
                      [sex, name, surname, country], 
                      [man, 'Vladimir', 'Mayakovsky',
                       ['Soviet Union']]).

:- end_tests(vd).











