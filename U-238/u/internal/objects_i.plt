:- use_module(u(internal/objects_i)).
:- use_module(u(v)).
:- use_module(library(aggregate)).

:- begin_tests(objects_i, [setup(objects_i_test_v)]).

test(class_fields) :-

   class_id(Id, citizen_v), !,
   
   class_fields(_:_, Id, false, false, F1),
   assertion(F1 = [height:_, name:_, sex:_, surname:_, weight:_]),

   class_fields(_:_, Id, false, true, F2),
   assertion(F2 = [class:_, functor:_]),

   class_fields(_:_, Id, true, false, F3),
   assertion(F3 = [birthday:_, country:_, id:_]),

   class_fields(_:_, Id, true, true, F4),
   assertion(F4 = [age:_]),

   class_fields(_:_, Id, false, _, F5),
   assertion(F5 = [class:_, functor:_, height:_, name:_, sex:_,
                   surname:_, weight:_]),

   class_fields(_:_, Id, true, _, F6),
   assertion(F6 = [age:_, birthday:_, country:_, id:_]),

   class_fields(_:_, Id, _, false, F7),
   assertion(F7 = [birthday:_, country:_, height:_, id:_, name:_,
                   sex:_, surname:_, weight:_]),

   class_fields(_:_, Id, _, true, F8),
   assertion(F8 = [age:_, class:_, functor:_]),

   class_fields(_:_, Id, _, _, F9),
   assertion(F9 = [age:_, birthday:_, class:_, country:_,
                   functor:_, height:_, id:_, name:_, sex:_,
                   surname:_,weight:_]).   


test(obj_unify_int_with_evals) :-

   class_primary_id(citizen_v, Class_Id),
   obj_construct(citizen_v, [], [], O),
   obj_unify_int(Class_Id, 
                 [functor, class, birthday], throw,
                 O,
                 [citizen_v, citizen_v, 1994], _),
   obj_field(O, class, Class1),
   assertion(Class1 == citizen_v),
   
   obj_unify_int(Class_Id,
                 [functor, class, sex], throw,
                 O,
                 [citizen_v, callup_v, man], _),
   obj_field(O, class, Class4),
   assertion(Class4 == callup_v),

   obj_unify_int(Class_Id,
                 [functor, class, birthday, sex], throw,
                 O,
                 [X, Y, 1994, man], _),
   obj_field(O, class, Class5),
   assertion(Class5 == callup_v),
   assertion(X = citizen_v),
   assertion(Y = callup_v).

test(obj_unify_int_with_evals_rebased) :-

   obj_construct(citizen_v, [], [], O0),
   obj_rebase((object_v -> db_object_v), O0, O),
   arg(1, O, Class_Id),
   obj_unify_int(Class_Id, 
                 [functor, class, birthday], throw,
                 O,
                 [citizen_v, citizen_v, 1994], _),
   obj_field(O, class, Class1),
   assertion(Class1 == citizen_v),
   
   obj_unify_int(Class_Id,
                 [functor, class, sex], throw,
                 O,
                 [citizen_v, callup_v, man], _),
   obj_field(O, class, Class4),
   assertion(Class4 == callup_v),

   obj_unify_int(Class_Id,
                 [functor, class, birthday, sex], throw,
                 O,
                 [X, Y, 1994, man], _),
   obj_field(O, class, Class5),
   assertion(Class5 == callup_v),
   assertion(X = citizen_v),
   assertion(Y = callup_v).


test(obj_unify_int_combinatoric_feature1,
     [N =:= N_Flds ** N_Flds]) :-

   obj_construct(objects_i_test_v, [], [], O),
   arg(1, O, Class_Id),
   class_fields(_, Class_Id, _, false, Flds),
   length(Flds, N_Flds),
   length(U_Flds, N_Flds),
   aggregate_all(count,
                 obj_unify_int(Class_Id, U_Flds, throw, O, U_Flds,
                               _),
                 N).

test(obj_unify_int_combinatoric_feature1_rebased,
     [N =:= N_Flds ** N_Flds]) :-

   obj_construct(objects_i_test_v, [], [], O0),
   obj_rebase((object_v -> db_object_v), O0, O),
   arg(1, O, Class_Id),
   class_fields(_, Class_Id, _, false, Flds),
   length(Flds, N_Flds),
   length(U_Flds, N_Flds),
   aggregate_all(count,
                 obj_unify_int(Class_Id, U_Flds, throw, O, U_Flds,
                               _),
                 N).

test(obj_unify_int_combinatoric_feature2,
     [N =:= N_Noneval_Flds ** N_Flds]) :-

   obj_construct(objects_i_test_v, [], [], O),
   arg(1, O, Class_Id),
   class_fields(_, Class_Id, _, _, Flds),
   class_fields(_, Class_Id, _, false, Noneval_Flds),
   length(Flds, N_Flds),
   length(U_Flds, N_Flds),
   aggregate_all(count,
                 obj_unify_int(Class_Id, U_Flds, throw, O, U_Flds,
                               _),
                 N),
   length(Noneval_Flds, N_Noneval_Flds).

test(obj_unify_int_combinatoric_feature2_rebased,
     [N =:= N_Noneval_Flds ** N_Flds]) :-

   obj_construct(objects_i_test_v, [], [], O0),
   obj_rebase((object_v -> db_object_v), O0, O),
   arg(1, O, Class_Id),
   class_fields(_, Class_Id, _, _, Flds),
   class_fields(_, Class_Id, _, false, Noneval_Flds),
   length(Flds, N_Flds),
   length(U_Flds, N_Flds),
   aggregate_all(count,
                 obj_unify_int(Class_Id, U_Flds, throw, O, U_Flds,
                               _),
                 N),
   length(Noneval_Flds, N_Noneval_Flds).

objects_i_test_v :-

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(objects_i_test_v, object_v, [a, b, c]).
   

:- end_tests(objects_i).
