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

test(get_keymaster1, fail) :-
   class_primary_id(tarjan_vertex_v, CI),
   get_keymaster(CI, _).
  
test(get_keymaster2, fail) :-
   class_primary_id(man_v, CI),
   get_keymaster(CI, _).
  
test(get_keymaster3, fail) :-
   obj_construct(tarjan_vertex_v, [], [], TV1),
   obj_rebase((object_v -> db_object_v), TV1, TV),
   arg(1, TV, CI),
   get_keymaster(CI, _).
  
test(get_keymaster4, [CI == KI]) :-
   obj_construct(man_v, [], [], Man1),
   obj_rebase((object_v -> db_object_v), Man1, Man),
   arg(1, Man, CI),
   get_keymaster(CI, KI).
  
test(list_inheritance_names1) :-

   class_primary_id(callup_v, Id),
   list_inheritance_names(Id, List),
   assertion(List == [object_base_v, object_v, man_v, citizen_v,
                      callup_v]).

test(list_inheritance_names2) :-

   class_primary_id(object_base_v, Id),
   list_inheritance_names(Id, List),
   assertion(List == [object_base_v]).

test(list_inheritance_names3) :-

   class_primary_id(man_v, Id1),
   class_primary_id(citizen_v, Id2),
   list_inheritance_names(Id1, Id2, List),
   assertion(List == [man_v, citizen_v]).

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

test(same_class) :-

   obj_construct(man_v, [], [], Obj1),
   obj_rebase((object_v -> db_object_v), Obj1, Obj2),
   obj_rebase((object_v -> tarjan_vertex_v), Obj2, Obj3),
   arg(1, Obj1, Id1),
   arg(1, Obj2, Id2),
   arg(1, Obj3, Id3),
   same_class(Id1, true, man_v),
   same_class(Id1, true, Class1),
   assertion(Class1 == man_v),
   \+ same_class(Id1, false, man_v),
   \+ same_class(Id1, false, _),
   same_class(Id2, false, man_v),
   same_class(Id2, false, Class2),
   assertion(Class2 == man_v),
   \+ same_class(Id2, true, man_v),
   \+ same_class(Id2, true, _),

   findall([NR, Class], same_class(Id1, NR, Class), L1),
   findall([NR, Class], same_class(Id3, NR, Class), L3),

   assertion(L1 == [[true, man_v]]),
   assertion(L3 == [[false, man_v]]).

test(descendant_class1) :-

   obj_construct(callup_v, [], [], Obj1),
   obj_rebase((object_v -> db_object_v), Obj1, Obj2),
   obj_rebase((object_v -> tarjan_vertex_v), Obj2, Obj3),
   arg(1, Obj1, Id1),
   arg(1, Obj3, Id3),
   descendant_class(Id1, true, man_v),
   findall(Class, descendant_class(Id1, true, Class), L1U),
   msort(L1U, L1),
   assertion(L1 = [citizen_v, man_v, object_base_v, object_v]),

   \+ descendant_class(Id1, false, man_v),
   \+ descendant_class(Id1, false, _),

   descendant_class(Id3, false, man_v),
   findall(Class, descendant_class(Id3, false, Class), L2U),
   msort(L2U, L2),
   assertion(L2 = [citizen_v, db_object_v, man_v,
                   object_base_v, object_v, tarjan_vertex_v]),


   \+ descendant_class(Id3, true, man_v),
   \+ descendant_class(Id3, true, _),

   findall(p(NR, Class), descendant_class(Id1, NR, Class), PL4),
   findall(p(NR, Class), descendant_class(Id3, NR, Class), PL5),

   maplist(arg(1), PL4, BL4U), sort(BL4U, B4),
   maplist(arg(2), PL4, L4U), msort(L4U, L4),
   maplist(arg(1), PL5, BL5U), sort(BL5U, B5),
   maplist(arg(2), PL5, L5U), msort(L5U, L5),

   assertion(B4 == [true]),
   assertion(L4 == L1),
   assertion(B5 == [false]),
   assertion(L5 == L2).

test(descendant_class2_1) :-
   class_primary_id(man_v, Man_Id),
   findall(Desc,
           (  descendant_class(Desc_Id, Man_Id),
              class_id(Desc_Id, Desc)
           ),
           Descendants),
   assertion(Descendants = [citizen_v, callup_v]).

test(descendant_class2_2) :-
   class_primary_id(man_v, Man_Id),
   findall(Anc,
           (  descendant_class(Man_Id, Anc_Id),
              class_id(Anc_Id, Anc)
           ),
           Ancestors),
   assertion(Ancestors = [object_v, object_base_v]).

test(same_or_descendant1) :-

   obj_construct(callup_v, [], [], Obj1),
   obj_rebase((object_v -> db_object_v), Obj1, Obj2),
   obj_rebase((object_v -> tarjan_vertex_v), Obj2, Obj3),
   arg(1, Obj1, Id1),
   arg(1, Obj3, Id3),
   same_or_descendant(Id1, true, man_v),
   same_or_descendant(Id1, true, callup_v),
   \+ same_or_descendant(Id1, true, db_object_v),
   findall(Class, same_or_descendant(Id1, true, Class), L1U),
   msort(L1U, L1),
   assertion(L1 = [callup_v, citizen_v, man_v, object_base_v, object_v]),

   \+ same_or_descendant(Id1, false, object_base_v),
   \+ same_or_descendant(Id1, false, callup_v),
   \+ same_or_descendant(Id1, false, _),

   same_or_descendant(Id3, false, object_base_v),
   same_or_descendant(Id3, false, db_object_v),
   same_or_descendant(Id3, false, tarjan_vertex_v),
   same_or_descendant(Id3, false, callup_v),
   findall(Class, same_or_descendant(Id3, false, Class), L2U),
   msort(L2U, L2),
   assertion(L2 = [callup_v, citizen_v, db_object_v, man_v,
                   object_base_v, object_v, tarjan_vertex_v]),


   \+ same_or_descendant(Id3, true, callup_v),
   \+ same_or_descendant(Id3, true, man_v),
   \+ same_or_descendant(Id3, true, _),

   findall(p(NR, Class), same_or_descendant(Id1, NR, Class), PL4),
   findall(p(NR, Class), same_or_descendant(Id3, NR, Class), PL5),

   maplist(arg(1), PL4, BL4U), sort(BL4U, B4),
   maplist(arg(2), PL4, L4U), msort(L4U, L4),
   maplist(arg(1), PL5, BL5U), sort(BL5U, B5),
   maplist(arg(2), PL5, L5U), msort(L5U, L5),

   assertion(B4 == [true]),
   assertion(L4 == L1),
   assertion(B5 == [false]),
   assertion(L5 == L2).

test(same_or_descendant2_1) :-
   class_primary_id(man_v, Man_Id),
   findall(Desc,
           (  same_or_descendant(Desc_Id, Man_Id),
              class_id(Desc_Id, Desc)
           ),
           Descendants),
   assertion(Descendants = [man_v, citizen_v, callup_v]).

test(same_or_descendant2_2) :-
   class_primary_id(man_v, Man_Id),
   findall(Anc,
           (  same_or_descendant(Man_Id, Anc_Id),
              class_id(Anc_Id, Anc)
           ),
           Ancestors),
   assertion(Ancestors = [object_base_v, object_v, man_v]).

test(same_or_descendant2_3) :-
   Class_id = 1,
   same_or_descendant(Class_id, Class_id).

objects_i_test_v :-

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(objects_i_test_v, object_v, [a, b, c]).


:- end_tests(objects_i).
