:- begin_tests(v).
:- use_module(u(v)).
:- use_module(u(internal/objects_i)).

test(class_create1) :-
   % test class_create/3 version

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b]),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   class_fields(citizen_v, CF), sort(CF, CFS),
   class_fields(class_create_test_v, CCTF), sort(CCTF, CCTFS),
   class_fields_new(class_create_test_v, CCTFN),
   assertion(CF == CFS),
   assertion(CCTF = CCTFS),
   assertion(CCTFN == [a, b, c]),

   ord_subtract(CCTF, CF, New_Fields),
   assertion(New_Fields == CCTFN),

   obj_field(CCTO, age, Age),
   integer(Age),

   obj_key(CCTO, Key),
   assertion(Key == [id]).

test(class_create2) :-
   % test class_create/4 version

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b],
                [birthday, a]),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   class_fields(citizen_v, CF), sort(CF, CFS),
   class_fields(class_create_test_v, CCTF), sort(CCTF, CCTFS),
   class_fields_new(class_create_test_v, CCTFN),
   assertion(CF == CFS),
   assertion(CCTF = CCTFS),
   assertion(CCTFN == [a, b, c]),

   ord_subtract(CCTF, CF, New_Fields),
   assertion(New_Fields == CCTFN),

   obj_field(CCTO, age, Age),
   integer(Age),
   
   obj_key(CCTO, Key),
   assertion(Key == [a, birthday]).

test(class_create_feature1) :-
   % It is a strange feature

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b], []),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   obj_key(CCTO, Key),
   assertion(Key == []).

test(class_descendant1,
     [List == [callup_v, citizen_v, class_create_test_v]]
    ) :-

  findall(X, class_descendant(man_v, X), List0),
  msort(List0, List).

test(class_descendant2,
     [List == []]
    ) :-

  findall(X, class_descendant(callup_v, X), List0),
  msort(List0, List).

test(class_same_or_descendant1,
     [List == [callup_v, citizen_v, class_create_test_v, man_v]]
    ) :-

  findall(X, class_same_or_descendant(man_v, X), List0),
  msort(List0, List).

test(class_same_or_descendant2,
     [List == [callup_v]]
    ) :-

  findall(X, class_same_or_descendant(callup_v, X), List).

test(obj_construct_with_evals1) :-

    obj_construct(citizen_v,
                  [functor, class, birthday],
                  [citizen_v, citizen_v, 1994], O1),
    obj_field(O1, class, Class1),
    assertion(Class1 == citizen_v),
    
    obj_construct(citizen_v,
                  [birthday, functor, class],
                  [1994, citizen_v, citizen_v], O2),
    obj_field(O2, class, Class2),
    assertion(Class2 == citizen_v),
    
    obj_construct(citizen_v,
                  [birthday, sex, functor, class],
                  [1994, man, citizen_v, callup_v], O3),
    obj_field(O3, class, Class3),
    assertion(Class3 == callup_v),

    obj_construct(citizen_v,
                  [functor, class, birthday, sex],
                  [citizen_v, callup_v, 1994, man], O4),
    obj_field(O4, class, Class4),
    assertion(Class4 == callup_v),

    obj_construct(citizen_v,
                  [functor, class, birthday, sex],
                  [X, Y, 1994, man], O5),
    obj_field(O5, class, Class5),
    assertion(Class5 == callup_v),
    assertion(X = citizen_v),
    assertion(Y = callup_v).

    
test(obj_construct_bug1) :-

   class_fields(man_v, Field_Names),
   obj_construct(man_v, Field_Names, Field_Names, Obj),
   Obj =.. [man_v, _|Field_Names2],

   assertion(Field_Names == Field_Names2).

test(obj_diff1,
     [Diff == [diff(name, 'Sergei', 'Artemiy')]]) :-

   obj_construct(man_v, [name, surname], ['Sergei', 'Lodyagin'], M1),
   obj_construct(man_v, [name, surname], ['Artemiy', 'Lodyagin'], M2),
   obj_diff(M1, M2, Diff).

test(obj_diff2,
     [Diff == [diff(name, 'Sergei', 'Artemiy')]]) :-

   obj_construct(man_v, [name, surname], ['Sergei', 'Lodyagin'], M1_0),
   obj_rebase((object_v -> db_object_v), M1_0, M1),
   obj_construct(man_v, [name, surname], ['Artemiy', 'Lodyagin'], M2),
   obj_diff(M1, M2, Diff).
  

% After some time it will always fails (because it depends
% on the current year).
test(obj_downcast1,
     [C1 =@= callup_v(Class_Id, 1994, _, _, _, _, _, man, _, _)]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], C0),
   obj_downcast(C0, C1),
   arg(1, C1, Class_Id).

test(obj_field1, [Flds == Vals]) :-

   class_fields(citizen_v, Flds),
   obj_construct(citizen_v, Flds, Flds, Obj),
   findall(Val, (obj_field(Obj, Fld, Val), member(Fld, Flds)), Vals).

test(obj_field2, [Flds3 == Flds4]) :-
% Each eval field must be evaluable only once

   class_fields(citizen_v, Flds),
   obj_construct(citizen_v, Flds, Flds, Obj),
   findall(Fld, obj_field(Obj, Fld, _), Flds2),
   sort(Flds2, Flds3),
   msort(Flds2, Flds4).

test(obj_field3, [Surname == 'Grisha']) :-

   obj_construct(man_v, [], [], Man),
   obj_field(Man, name, Name),
   obj_field(Man, surname, Name),
   Name = 'Grisha',
   obj_field(Man, surname, Surname).
  
test(obj_field_bug1, [fail]) :-

   obj_construct(citizen_v, [sex], [woman], Obj),
   obj_field(Obj, sex, man).

test(obj_set_field1, [Surname =@= _]) :-

   obj_construct(man_v, [], [], Man),
   obj_set_field(Man, name, Name),
   obj_set_field(Man, surname, Name),
   Name = 'Grisha',
   obj_field(Man, surname, Surname).
  
test(obj_set_field2, [Surname =@= 'Grisha']) :-

   obj_construct(man_v, [name, surname], [Name, Name], Man),
   obj_set_field(Man, name, 'Grisha'),
   obj_field(Man, surname, Surname).
  
test(obj_key1) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Man1),
   obj_key(Man1, Key1),
   assertion(Key1 == [name, surname]),
   obj_key_value(Man1, Key_Value1),
   assertion(Key_Value1 =@= ['Adam', _]),

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man2),
   obj_key(Man2, Key2),
   assertion(Key2 == [id]),
   obj_key_value(Man2, Key_Value2),
   assertion(Key_Value2 =@= [_]).

test(obj_key2) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Man1_0),
   obj_rebase((object_v -> db_object_v), Man1_0, Man1),
   obj_key(Man1, Key1),
   assertion(Key1 == [name, surname]),
   obj_key_value(Man1, Key_Value1),
   assertion(Key_Value1 =@= ['Adam', _]),

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man2_0),
   obj_rebase((object_v -> db_object_v), Man2_0, Man2),
   obj_key(Man2, Key2),
   assertion(Key2 == [id]),
   obj_key_value(Man2, Key_Value2),
   assertion(Key_Value2 =@= [_]).

 
test(obj_rebase_bug1) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_rebase_bug2) :-

   obj_construct(citizen_v, [sex], [man], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [sex], [man], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_rebase_bug3_1) :-

   obj_construct(db_object_v, [], [], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(db_object_v, [], [], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =\= Class_Id3),

   class_primary_id(object_v, Object_V_Id),
   assertion(Obj3 == object_v(Object_V_Id)).

test(obj_rebase_bug3_2) :-

   obj_construct(citizen_v, [], [], Obj1_0),
   obj_rebase((man_v -> citizen_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [], [], Obj2_0),
   obj_rebase((object_v -> man_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2).

test(obj_rebase_bug4,
     [throws(error(cant_rebase_to_object_base_v,
                   context(obj_rebase/3, _)))]) :-

   obj_construct(citizen_v, [], [], Obj1_0),
   obj_rebase((citizen_v -> object_base_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [], [], Obj2_0),
   obj_rebase((citizen_v -> object_base_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((object_base_v -> citizen_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_reset_fields1) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields([name, height], Man0, Man1),
   obj_class_id(Man1, Class_Id),
   assertion(Man1 =@= man_v(Class_Id, _, _, man, _, 63)),
   
   obj_reset_fields([weight], Man1, Man2),
   assertion(Man2 =@= man_v(Class_Id, _, _, man, _, _)),
   
   obj_reset_fields([], Man2, Man3),
   assertion(Man3 =@= Man2).
   
test(obj_reset_fields2, [fail]) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields([name, height, age], Man0, _).
   
test(obj_reset_fields_weak) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields_weak([name, height, age], Man0, Man1),
   obj_class_id(Man1, Class_Id),
   assertion(Man1 =@= man_v(Class_Id, _, _, man, _, 63)).

test(obj_reset_fields_with_evals, [fail]) :-

   obj_construct(citizen_v, [sex], [man], O),
   obj_reset_fields([age], O, _).

test(obj_rewrite) :-

   obj_construct(man_v,
                 [name, height, weight],
                 ['Luda', 1.40, 99], Luda1),
   obj_rewrite(Luda1,
               [height, weight, name],
               [Old_Height, Old_Weight, Name],
               [1.44, 69, Name],
               Luda2),
   assertion([Old_Height, Old_Weight] =@= [1.40, 99]),
   obj_rewrite(Luda2,
               [height, weight, name],
               Luda2_Pars,
               [1.45, 64, _],
               Luda3),
   assertion(Luda2_Pars =@= [1.44, 69, 'Luda']),
   named_args_unify(Luda3, [height, weight, name], Luda3_Pars),
   assertion(Luda3_Pars =@= [1.45, 64, _]).

test(obj_rewrite_with_evals1, [X == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [X], [callup_v], _).

test(obj_rewrite_with_evals2, [fail]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [_], [citizen_v], _).

test(obj_rewrite_with_evals3, [X == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [X], [X], _).

test(obj_rewrite_with_evals4) :-
% <NB> can't reset eval field, compare with test(obj_rewrite)
   
   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [_], [_], _).

test(class_fields) :-

   class_fields(object_base_v, Object_Base_V_Fields),
   assertion(Object_Base_V_Fields == []),

   class_fields_new(object_base_v, Object_Base_V_New_Fields),
   assertion(Object_Base_V_New_Fields == []),
   
   class_fields(object_v, Object_V_Fields),
   assertion(Object_V_Fields == []),

   class_fields_new(object_v, Object_V_New_Fields),
   assertion(Object_V_New_Fields == []),
   
   class_fields(man_v, Man_V_Fields),
   assertion(Man_V_Fields == [height, name, sex, surname, weight]),

   class_fields_new(man_v, Man_V_New_Fields),
   assertion(Man_V_New_Fields == [height, name, sex, surname,
                                  weight]),
   
   class_fields(citizen_v, Citizen_V_Fields),
   assertion(Citizen_V_Fields == [birthday, country, height, id,
                                  name, sex, surname, weight]),

   class_fields_new(citizen_v, Citizen_V_New_Fields),
   assertion(Citizen_V_New_Fields == [birthday, country, id]).


% After some time it will always fails (because it depends
% on the current year, see The Uranium Book).
test(eval_fields1, [Class == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], C), 
   obj_field(C, class, Class).


test(cleanup_obj_rebase, [N1 =:= N2]) :-

   aggregate(count, A^B^C^(objects:class_id(A, B, C)), N1),
   catch((obj_construct(numeric_id_v, [], [], Obj1),
          obj_rebase((object_v -> citizen_v), Obj1, _)),
         E, true),
   E = error(EX, _), functor(EX, duplicate_field, _),
   aggregate(count, A^B^C^(objects:class_id(A, B, C)), N2).


:- end_tests(v).
