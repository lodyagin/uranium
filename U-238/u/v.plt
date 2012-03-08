:- begin_tests(v).
:- use_module(u(v)).
:- use_module(u(internal/objects_i)).

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
   
test(obj_construct_bug1) :-

   class_fields(man_v, Field_Names),
   obj_construct(man_v, Field_Names, Field_Names, Obj),
   Obj =.. [man_v, _|Field_Names2],

   assertion(Field_Names == Field_Names2).

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
  
test(obj_field_bug1, [fail]) :-

   obj_construct(citizen_v, [sex], [woman], Obj),
   obj_field(Obj, sex, man).

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


% After some time it will always fails (because it depends
% on the current year, see The Uranium Book).
test(eval_fields1, [Class == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], C), 
   obj_field(C, class, Class).

:- end_tests(v).
