:- begin_tests(vd, [setup(setup)]).

:- multifile prolog:message//1.

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/objects_i)).
:- use_module(u(util/lambda)).
:- use_module(library(aggregate)).
:- use_module(library(ordsets)).

test(clear_test_db, [fail, setup(db_clear(people))]) :-

	db_recorded(people, _).

test(db_clear, [setup(model_db), List == []]) :-

   reload_classes,

   % clear all DBs with citizen_v
   (  db_name(DB_Key),
      (  named_args_unify(DB_Key, citizen_v, [], [], _)
      -> db_clear(DB_Key)
      ;  true
      ),
      fail ; true
   ),

   findall(X,
           (db_i:current_predicate(citizen_v, X), call(db_i:X)),
           List).

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

test(db_iterate5, [setup(model_db), N =:= 3]) :-

   findall(O, db_iterate(people, functor(_), O), L),
   length(L, N).

test(db_iterate6, [setup(model_db), N =:= 3]) :-

   findall(O, db_iterate(people, same_or_descendant(man_v), O), L),
   length(L, N).

test(db_iterate7, [setup(model_db), N =:= 1]) :-

   findall(O, db_iterate(people, same_or_descendant(citizen_v), O), L),
   length(L, N).

test(db_iterate8, [setup(model_db)]) :-

   findall(Surname, db_iterate(people, surname(Surname), _), SL),
   assertion(SL =@= [_, _, 'Mayakovsky']),
   findall(Surname, db_iterate(people, sex(Surname), _), SL2),
   assertion(SL2 == [man, woman, man]).

test(db_iterate9, [setup(model_db), N == 1]) :-

   aggregate_all(count, db_iterate(people, surname(+bound), _), N).

test(db_iterate10, [setup(model_db), N == 2]) :-

   aggregate_all(count, db_iterate(people, surname(+free), _), N).

test(db_iterate11, [setup(model_db)]) :-

   aggregate_all(count,
                 db_iterate(people,
                            surname('Mayakovsky') /\ surname(+free), _),
                 N1),
   assertion(N1 == 0),
   aggregate_all(count,
                 db_iterate(people,
                            surname(+free) /\ surname('Mayakovsky'), _),
                 N2),
   assertion(N2 == 0),
   aggregate_all(count,
                 db_iterate(people,
                            surname(+free) /\ surname(+bound), _),
                 N3),
   assertion(N3 == 0),
   aggregate_all(count,
                 db_iterate(people,
                            surname(+free) \/ surname(+bound), _),
                 N4),
   assertion(N4 == 3),
   aggregate_all(count,
                 db_iterate(people,
                            surname('Mayakovsky') /\ surname(+bound), _),
                 N5),
   assertion(N5 == 1),
   aggregate_all(count,
                 db_iterate(people,
                            surname(+bound) /\ surname('Mayakovsky'), _),
                 N6),
   assertion(N6 == 1).


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

test(db_put_object1,
     [setup(model_db),
      List == [['Adam'], ['Eva'], ['Moses'], ['Vladimir']]
      ]) :-

   obj_construct(man_v, [name], ['Moses'], Man),
   db_put_object(people, Man),
   db_select_list(people, _, [name], List).

test(db_put_object2, [setup(model_db)]) :-

   obj_construct(man_v, [name], ['Moses'], Man0),
   obj_rebase((object_v -> db_object_v), Man0, Man),
   db_put_object(people, Man).

test(db_put_object3,
     [setup(model_db),
      error(db_obj_replace_protector(people2, _, Man))]) :-

   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man), !,
   db_put_object(people2, Man).

test(db_put_object4, [setup(model_db)]) :-
% replacing

   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man0), !,
   obj_rewrite(Man0, [name], ['Moses'], [_], Man1),
   db_put_object(people2, throw, Man1, Man2, Replaced),
   named_args_unify(Man2,
                    [name, db_key, db_ref],
                    [New_Name, DB_Key, DB_Ref]),
   assertion(New_Name =@= _),
   assertion(Replaced == replaced),
   assertion(nonvar(DB_Ref)),
   assertion(DB_Key == people2),

   db_size(people2, N),
   assertion(N =:= 1).

test(db_put_object5,
     [setup(model_db),
      error(domain_error(unbound_db_ref, DB_Ref))]) :-

   db_construct(people, man_v, [name], ['Moses']),
   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man0), !,
   obj_field(Man0, db_ref, DB_Ref),
   db_put_object(people, throw, Man0, _, _).

test(db_put_object6,
     [setup(model_db),
      error(domain_error(unbound_or_same_db_key, people2))]) :-

   db_construct(people, man_v, [name], ['Moses']),
   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man0), !,
   obj_reset_fields([db_ref], Man0, Man),
   db_put_object(people, throw, Man, _, _).

test(db_put_object7,
     [setup(model_db),
      error(db_obj_replace_protector(people2, replaced, Man1))]) :-

   db_construct(people2, man_v, [name], ['Moses']),
   db_recorded(people2, Man0), !,
   obj_rewrite(Man0, [db_ref, name], [_, 'Moses'], [_, _], Man1),
   db_put_object(people2, throw, Man1, _, replaced).
   % does not replace because of unbound db_ref

test(db_put_object8,
     [setup(model_db),
      error(db_obj_replace_protector(people2, replaced, Man))]) :-

   db_construct(people2, man_v, [name], ['Moses']),
   obj_construct(man_v, [name], ['Simeon'], Man),
   db_put_object(people2, _, Man, _, replaced).
   % does not replace because of not db_object_v descendant

test(db_put_object9,
     [blocked(rebase_collision), setup(model_db),
      List == [['Adam'], ['Eva'], ['Moses'], ['Vladimir']]
      ]) :-

   obj_construct(man_v, [name], ['Moses'], Man0),
   obj_rebase((object_v -> db_object_v), Man0, Man1),
   obj_rebase((object_v -> tarjan_vertex_v), Man1, Man),
   db_put_object(people, Man),
   db_select_list(people, _, [name], List).

test(db_recorda1,
     [setup(model_db),
      List == [['Moses'], ['Adam'], ['Eva'], ['Vladimir']]
      ]) :-

   obj_construct(man_v, [name], ['Moses'], Man),
   db_recorda(people, Man),
   db_select_list(people, _, [name], List).

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


% test(db_rewrite1,
%      [setup(model_db),
%       List =@= [['Adam', 'Kasperski', 66],
%                 ['Eva', 'Kasperski', 66],
%                 ['Vladimir', 'Kasperski', 66]]
%      ]) :-

%    db_rewrite(people, _,
%               [weight, surname], _, [66, 'Kasperski']),
%    db_select_list(people, _, [name, surname, weight], List).

% test(db_rewrite2,
%      [setup(model_db),
%       List =@= [['Adam', 'Kasperski', 66],
%                 ['Eva', 'Kasperski', 66],
%                 ['Vladimir', 'Mayakovsky', _]]
%      ]) :-

%    db_rewrite(people, man_v,
%               [weight, surname], _, [66, 'Kasperski']),
%    db_select_list(people, _, [name, surname, weight], List).

test(db_search,
     [setup(model_db),
      List == [['Adam'], ['Vladimir']]
     ]) :-

   db_search(people, people2, db_search_pred),
   db_select_list(people2, _, [name], List).

db_search_pred(Man) :-

  obj_field(Man, sex, Sex),
  Sex == man.

test(db_select1,
     [setup(model_db),
      List =@= [[man_v, man, 'Adam', _, 73, _],
                [man_v, woman, 'Eva', _, 64, _],
                [citizen_v, man, 'Vladimir', 'Mayakovsky', _,
                 ['Soviet Union']]]
      ]) :-

   findall(Row,
           db_select(people,
                     [class, sex, name, surname, weight, country],
                     Row),
           List
          ).

test(db_select2,
     [setup(model_db),
      List =@= [[man_v, man, 'Adam', _, 73, _],
                [man_v, woman, 'Eva', _, 64, _]]
      ]) :-

   findall(Row,
           (   Row = [man_v|_],
               db_select(people,
                         [functor, sex, name, surname, weight,
                          country],
                         Row)
           ), List
          ).


test(db_select_list1,
     [setup(model_db),
      List =@= [[man_v, man, 'Adam', _, 73, _],
                [man_v, woman, 'Eva', _, 64, _],
                [citizen_v, man, 'Vladimir', 'Mayakovsky', _,
                 ['Soviet Union']]]
      ]) :-

   db_select_list(people, _,
                  [class, sex, name, surname, weight, country],
                  List).

test(db_select_list2,
     [setup(model_db),
      List =@= [[man_v, man, 'Adam', _, 73, _],
                [man_v, woman, 'Eva', _, 64, _]]
      ]) :-

   db_select_list(people, man_v, _,
                  [class, sex, name, surname, weight, country],
                  List).

test(db_select_list3,
     [setup(model_db),
      error(no_object_field(_, country))
      ]) :-

   db_select_list(people, man_v, throw,
                  [class, sex, name, surname, weight, country],
                  _).

test(filter_on_db,
     [setup(model_db),
      List = [['Adam'], ['Vladimir']]
     ]) :-

   filter_on_db(people, [sex], [man]),
   db_select_list(people, _, [name], List).


test(named_args_unify1, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [sex], [man], _),
           ['.', '.']).

test(named_args_unify2, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [sex, sex], [man, man], _),
           ['.', '.']).

test(named_args_unify3_eval, [setup(model_db)]) :-

   findall('.',
           named_args_unify(people, _, [sex, age], [_, _], _),
           ['.']).

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

test(key_rule12,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [], []).

test(key_rule13,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [name], ['James']).

test(key_rule14,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v, [], []),
   db_construct(people, citizen_v, [id], [5]).

test(key_rule15,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, citizen_v, [id], [4]),
   db_construct(people, man_v,
                [name, surname], ['James', 'Crick']).

test(key_rule16,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v, [name], ['James']),
   db_construct(people, citizen_v, [id], [4]).

test(key_test1) :-
   obj_construct(c4_v, [a, b, c], [1, 2, 3], A),
   obj_construct(c4_v, [a, b, c], [1, 20, 30], B),
   key_test2(A, B, allow).

% nearest_keymaster(A) = nearest_keymaster(B)
test(key_test_set1) :-

   key_test(c1_v, c2_v).

% nearest_keymaster(A) is_accessor_off nearest_keymaster(B)
test(key_test_set2) :-

   key_test(c1_v, c3_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(c1_v, c4_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(c1_v, c5_v), % key(nk(A)) = key(nk(B))
   key_test(c1_v, c6_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(c1_v, c7_v). % key(nk(A)) instersect key(nk(B)) = {}

% nearest_keymaster(A) is_desc_off nearest_keymaster(B)
test(key_test_set3) :-

   key_test(c4_v, c1_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(c3_v, c1_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(c5_v, c1_v), % key(nk(A)) = key(nk(B))
   key_test(c6_v, c1_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(c7_v, c1_v). % key(nk(A)) instersect key(nk(B)) = {}

% nk(A) no_parent_relation_to nk(B) /\ key(nk(A, B)) = {}
test(key_test_set4_1) :-

   key_test(c1_v, d2_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(c1_v, d3_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(c1_v, d4_v), % key(nk(A)) = key(nk(B))
   key_test(c1_v, d5_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(c1_v, d6_v). % key(nk(A)) instersect key(nk(B)) = {}

% nk(A) no_parent_relation_to nk(B) /\ key(nk(A, B)) /= {}
test(key_test_set4_2) :-

   key_test(c5_v, e2_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(c5_v, e3_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(c5_v, e4_v), % key(nk(A)) = key(nk(B))
   key_test(c5_v, e5_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(c5_v, e6_v). % key(nk(A)) instersect key(nk(B)) = {}

% nk(A) no_parent_relation_to nk(B) /\ key(nk(A, B)) = key(nk(A))
test(key_test_set4_3) :-

   key_test(c5_v, f2_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(c5_v, f3_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(c5_v, f4_v), % key(nk(A)) = key(nk(B))
   key_test(c5_v, f5_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(c5_v, f6_v). % key(nk(A)) instersect key(nk(B)) = {}

% nk(A) no_parent_relation_to nk(B) /\ key(nk(A, B)) = key(nk(B))
test(key_test_set4_4) :-

   key_test(f3_v, c5_v), % key(nk(A)) is_superset_of key(nk(B))
   key_test(f2_v, c5_v), % key(nk(A)) is_subset_of key(nk(B))
   key_test(f4_v, c5_v), % key(nk(A)) = key(nk(B))
   key_test(f5_v, c5_v), % key(nk(A)) instersect key(nk(B)) /= {}
   key_test(f6_v, c5_v). % key(nk(A)) instersect key(nk(B)) = {}

test(db_properties_v1_overwrite, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   db_construct(people, db_properties_v,
                [key_policy], [overwrite]),
   db_construct(people, man_v, [surname], ['Lodyagin']),

   findall([Name, Surname],
           named_args_unify(people, _, [name, surname],
                            [Name, Surname], _),
           List1),
   assertion(List1 =@= [[_, 'Lodyagin']]).

test(db_properties_v2_throw,
     [setup(db_clear(people)),
      error(db_key_exists(people, _, _))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   db_construct(people, db_properties_v,
                [key_policy], [overwrite]),
   db_construct(people, man_v, [surname], ['Lodyagin']),
   db_construct(people, db_properties_v,
                [key_policy], [throw]),
   db_construct(people, man_v, [name], ['Sergei']).

test(db_properties_v3_fail_ignore, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin'], Obj1),
   db_construct(people, db_properties_v,
                [key_policy], [fail]),
   \+ db_construct(people, man_v, [surname], ['Lodyagin']),

   named_args_unify(people, db_properties_v, [], _, Singl1), !,
   obj_rewrite(Singl1, [key_policy], [fail], [ignore], Singl2),
   db_put_object(people, _, Singl2, _, replaced),

   db_construct(people, man_v,
                [name, surname, height], ['Sergei', 'Lodyagin',
                1.74], Obj2),

   findall([Name, Surname, Height],
           named_args_unify(people, _,
                            [name, surname, height],
                            [Name, Surname, Height], _),
           List1),
   assertion(List1 =@= [['Sergei', 'Lodyagin', _]]),
   assertion(Obj1 =@= Obj2).

test(db_properties_v4_fail_ignore2, [setup(db_clear(people))]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin'], Obj1),
   db_construct(people, db_properties_v,
                [key_policy], [fail]),
   \+ db_construct(people, man_v, [surname], ['Lodyagin']),

   obj_construct(db_properties_v, [key_policy], [ignore], Singl2),
   db_put_object(people, overwrite, Singl2, _),

   db_construct(people, man_v,
                [name, surname, height], ['Sergei', 'Lodyagin',
                1.74], Obj2),

   findall([Name, Surname, Height],
           named_args_unify(people, _,
                            [name, surname, height],
                            [Name, Surname, Height], _),
           List1),
   assertion(List1 =@= [['Sergei', 'Lodyagin', _]]),
   assertion(Obj1 =@= Obj2).

test(db_properties_v5_invalid_policy,
     [setup(db_clear(people)),
      blocked(need_to_see)
      ]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),
   db_construct(people, db_properties_v,
                [key_policy], [throws]),
   db_construct(people, man_v, [surname], ['Lodyagin']).


test(rebase_collision_bug1, [blocked(rebase_collision)]) :-

   obj_construct(man_v, [], [], O0),
   obj_rebase((object_v -> tarjan_vertex_v), O0, O),
   db_put_object(people, O).

% check default key policy
test(after_put_callback1,
     [setup(callback_setup),
      error(db_key_exists(people, _, _))
     ]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),

   context_module(This),
   db_construct(people, db_properties_v,
                [after_put_callback], [This:after_put_callback]),

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']).


test(after_put_callback2,
     [setup(callback_setup)]) :-

   db_construct(people, man_v,
                [name, surname], ['Sergei', 'Lodyagin']),

   context_module(This),
   db_construct(people, db_properties_v,
                [after_put_callback], [This:after_put_callback], Props),

   db_construct(people, man_v,
                [name, surname], ['Alex', 'Petrenko']),

   obj_rewrite(Props, [key_policy], _, [fail], Props2),
   db_put_object(people, _, Props2, Props3, replaced),
   ignore(db_construct(people, man_v,
                       [name, surname], ['Alex', 'Petrenko'])),

   obj_rewrite(Props3, [after_put_callback], _, [_], Props4),
   db_put_object(people, _, Props4, _, replaced),

   db_construct(people, man_v,
                [name, surname], ['James', 'Petrenko']),

   findall(p(Functor, Name, Surname),
           (  apc(Obj),
              obj_unify(Obj, weak,
                        [functor, name, surname], [Functor, Name, Surname])
           ),
           List),

   assertion(List = [p(db_properties_v, _, _),
                     p(man_v, 'Alex', 'Petrenko'),
                     p(db_properties_v, _, _),
                     p(db_properties_v, _, _),
                     p(man_v, 'James', 'Petrenko')
                     ]).

after_put_callback(Obj) :-

   assertz(apc(Obj)).

callback_setup :-

   db_clear(people),
   retractall(apc(_)).

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

setup :-

   db_clear(plunit_vd__key_test),
   reload_classes.

reload_classes :-

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),
   key_test_classes.

key_test_classes :-

   class_create(c0_v, object_v, [a, b, c], [a]),
   class_create(c1_v, c0_v, [], [b, c]),
   class_create(c2_v, c1_v, [d]),
   class_create(c3_v, c2_v, [], [c]),
   class_create(c4_v, c3_v, [], [a, b, c]),
   class_create(c5_v, c1_v, [], [b, c]),
   class_create(c6_v, c1_v, [d], [c, d]),
   class_create(c7_v, c1_v, [e], [a, e]),

   class_create(d1_v, object_v, [a, b, c], [a]),
   class_create(d2_v, d1_v, [], [b]),
   class_create(d3_v, d1_v, [], [a, b, c]),
   class_create(d4_v, d3_v, [], [b, c]),
   class_create(d5_v, d2_v, [], [a, b]),
   class_create(d6_v, d2_v, [f], [c, f]),

   class_create(e1_v, c0_v, [], [b]),
   class_create(e2_v, e1_v, [], [b]),
   class_create(e3_v, e1_v, [], [a, b, c]),
   class_create(e4_v, e3_v, [], [b, c]),
   class_create(e5_v, e2_v, [], [a, b]),
   class_create(e6_v, e2_v, [g], [c, g]),

   class_create(f1_v, c1_v, [], [a]),
   class_create(f2_v, f1_v, [], [b]),
   class_create(f3_v, f1_v, [], [a, b, c]),
   class_create(f4_v, f3_v, [], [b, c]),
   class_create(f5_v, f2_v, [], [a, b]),
   class_create(f6_v, f2_v, [h, j], [j, h]).

key_pattern(patt(Left, Common, Right)) :-
   L = [free, bound],
   member(Left, L),
   member(Common, L),
   member(Right, L).

key_test(AC, BC) :-
   class_primary_id(AC, Id1),
   class_primary_id(BC, Id2),
   nearest_common_keymaster_int(Id1, Id2, Keymaster_Id),
   forall(
          key_pattern(Patt),
          (   fill_keys(AC, BC, Keymaster_Id, Patt, A, B),
              assertion(key_test2(A, B))
	  )).

% Calculate the result
key_test_result(A, B, allow) :-
   ( obj_key(A, []) ; obj_key(B, [])), !.
key_test_result(A, B, conflict) :-
   arg(1, A, Id1),
   arg(1, B, Id2),
   nearest_common_keymaster_int(Id1, Id2, Keymaster_Id),
   get_key(Keymaster_Id, K_Key),
   K_Key \= [],
   obj_unify(A, K_Key, Value),
   obj_unify(B, K_Key, Value), !.
key_test_result(_, _, allow).

key_test2(A, B) :-
   key_test_result(A, B, Conflict),
   key_test2(A, B, Conflict).

key_test2(A, B, allow) :-
   DB = plunit_vd__key_test,
   db_clear(DB),
   db_put_object(DB, A),
   db_put_object(DB, fail, B, _),
   db_clear(DB),
   db_put_object(DB, B),
   db_put_object(DB, fail, A, _), !.

key_test2(A, B, conflict) :-
   DB = plunit_vd__key_test,
   db_clear(DB),
   db_put_object(DB, A),
   \+ db_put_object(DB, fail, B, _),
   db_clear(DB),
   db_put_object(DB, B),
   \+ db_put_object(DB, fail, A, _).

fill_keys(AC, BC, Keymaster_Id, Patt, A, B) :-
   get_key(Keymaster_Id, Common_Key_Part),
   obj_construct(AC, [], [], A),
   obj_construct(BC, [], [], B),
   fill_key(Patt, Common_Key_Part, A, B).

fill_key(patt(A_Patt, Common_Patt, B_Patt), Common_Key, A, B) :-
   obj_key(A, A_Key),
   obj_unify(A, A_Key, A_Key_Value),
   fill_key(A_Patt, A_Key, A_Key_Value),
   obj_unify(A, Common_Key, Common_Key_Value),
   fill_key(Common_Patt, Common_Key, Common_Key_Value),
   obj_key(B, B_Key),
   obj_unify(B, B_Key, B_Key_Value),
   fill_key(B_Patt, B_Key, B_Key_Value).


fill_key(bound, Key, Key) :- !.
fill_key(half_bound, [K|KT], [K|KVT]) :- !,
   same_length(KT, KVT).
fill_key(free, Key, Key_Value) :-
   same_length(Key, Key_Value).

:- end_tests(vd).












