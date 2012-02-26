:- begin_tests(v).
:- use_module(u(v)).

test(obj_rebase_bug1) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2).

test(obj_construct_bug1) :-

   class_fields(man_v, Field_Names),
   obj_construct(man_v, Field_Names, Field_Names, Obj),
   Obj =.. [man_v, _|Field_Names2],

   assertion(Field_Names == Field_Names2).

:- end_tests(v).
