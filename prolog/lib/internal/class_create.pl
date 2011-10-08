:- module(class_create,
          [class_create/3,
           class_create/4
          ]).

:- use_module(library(internal/objects_i)).
:- use_module(library(internal/check_arg)).

%
% class_create(+Class, +Parent, +Add_Fields)
%
% Assert the new Class definition into the objects module
%

class_create(Class, Parent, Fields) :-

  Ctx = context(class_create/3, _), %FIXME when called from /4
  (  (var(Class) ; var(Parent); var(Fields))
  -> throw(error(instantiation_error, Ctx))
  ;  true ),
  check_class_arg(Class, Ctx),
  check_class_arg(Parent, Ctx),
  (  is_list(Fields) -> true
  ;  throw(error(type_error(list, Fields), Ctx))
  ),
  (  class_primary_id(Parent, Parent_Id)
  -> true
  ;  throw(error(existence_error(uranium_class, Parent), Ctx))
  ),
  assert_new_class(Class, Parent_Id, Fields, Ctx),
  get_key(Parent_Id, Parent_Key),
  class_primary_id(Class, Class_Id),
  assert_key(Class_Id, Parent_Key),
  assert_copy(Class_Id, Parent).
  
%
% class_create(+Class, +Parent, +Add_Fields, +Key)
%
% Assert the new Class definition into the objects module,
% set a (compound) key to Key
%

class_create(Class, Parent, Fields, New_Key) :-

  class_create(Class, Parent, Fields),
  
  (  var(New_Key)
  -> throw(error(instantiation_error,
                 context(class_create/4, _)))
  ;  \+ is_list(New_Key)
  -> throw(error(type_error(list, New_Key),
                 context(class_create/4, _)))
  ;  \+ is_set(New_Key)  
  -> throw(error(domain_error(no_duplicates, New_Key),
                 context(class_create/4, _)))
  ;  % check the New_Key contents
     fields_names_types(New_Key, New_Key_Simpl, _),
     list_to_ord_set(New_Key_Simpl, New_Key_Set)
  ),
  class_primary_id(Parent, Parent_Id),
  get_key(Parent_Id, Parent_Key_Set),
  ord_union(Parent_Key_Set, New_Key_Set, Key_Set),
  class_primary_id(Class, Class_Id),
  assert_key(Class_Id, Key_Set).


assert_new_class(Class, Parent_Id, Fields, Ctx) :-

  (  class_primary_id(Class, Class_Id)
  -> true % class id can be already created by object_module
  ;  gen_class_id(Class, Class_Id) ),

  % arity/2 :- true is only asserted by this module
  (  objects:clause(arity(Class_Id, _), _) 
  -> throw(class_exists(Class))
  ;  true
  ),

  (  fields_names_types(Fields, Field_Names, _)
  -> true
  ;  throw(error(type_error(object_fields_and_types,Fields), Ctx))
  ),
  (  is_set(Field_Names) -> true
  ;  Ctx = context(_, 'duplicates were found'),
     throw(error(domain_error(object_fields, Field_Names), Ctx))
  ),
                             
  (  \+ class_id(Class_Id, _)
  -> objects:assertz(class_id(Class_Id, true, Class))
  ;  true ),  

  % assert parent
  objects:assertz(parent(Class_Id, Parent_Id)),
  
  assert_class_fields(Class_Id, Fields, _, Next_Arg),
  Arity is Next_Arg - 1,

  % check no repeats in the class field names
  bagof(Field_Name,
        T1^T2^T3^T4^T5^(objects:clause(
          field(Class_Id, Field_Name, T1, T2, T3, T4), T5),
          functor(T5, arg, _)),
        All_Field_Names),
  (  \+ is_set(All_Field_Names)
  -> % delete incorrect definitions
     % FIXME remove class cleanup to catch
     objects:retractall(field(Class_Id, _, _, _, _, _)),
     throw(error(duplicate_field(All_Field_Names), Ctx))
  ;  true
  ), !,

  % if all ok make final asserts
  objects:assertz(fields(Class_Id, All_Field_Names)),
  objects:assertz(arity(Class_Id, Arity)).


assert_class_fields(Class_Id, Top_Fields, Arg0, Arg) :-

   assert_inherited_fields(Class_Id, Class_Id, Arg0, Arg1),
   assert_class_fields2(Class_Id, Class_Id, Top_Fields, Arg1, Arg).

% reset Arg to 2 (always 1 holds class id)
assert_inherited_fields(_, 0, _, 2) :- !.

assert_inherited_fields(Class_Id, Ref_Class_Id, Arg0, Arg) :-

   % process parent's fields
   objects:parent(Ref_Class_Id, Parent_Id), !,
   assert_inherited_fields(Class_Id, Parent_Id, Arg0, Arg1),

   class_fields(Parent_Id, true, New_Fields),
   assert_class_fields2(Class_Id, Parent_Id, New_Fields,
                        Arg1, Arg).
   

assert_class_fields2(_, _, [], Arg, Arg) :- !. 


assert_class_fields2(Class_Id, Native_Id,
                    [Field_Name:Field_Type|FT], Arg0, Arg) :- !,

   (  Class_Id =\= Native_Id
   -> % re-assert inherited field with new class id
      objects:clause(
         field(Native_Id, Field_Name, Obj, Value, Field_Type,
            true), %true marks fields introduced by Native_Id
         Body),
      (  functor(Body, arg, 3)
      -> % it is not eval field
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false) :- arg(Arg0, Obj, Value))
         ),
         Arg1 is Arg0 + 1
      ;  % eval fields not depends on Arg
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false) :- Body)
         ),
         Arg1 = Arg0
      )
   ;  % assert new fields
      % NB new evaluated fields are not in this list
      objects:assertz(
        (field(Class_Id, Field_Name, Obj, Value, Field_Type,
            true) :- arg(Arg0, Obj, Value))
      ),
      Arg1 is Arg0 + 1
   ),

   assert_class_fields2(Class_Id, Native_Id, FT, Arg1, Arg).

assert_class_fields2(Class_Id, Native_Id,
                    [Field_Name|FT], Arg0, Arg) :-

   assert_class_fields2(Class_Id, Native_Id,
                        [Field_Name:_|FT], Arg0, Arg).

                             
assert_key(_, []) :- !.

assert_key(Class_Id, Keys) :-

  assertz(objects:key(Class_Id, Keys)).


%
% Inherit copy from parent if not defined for Class
%

assert_copy(Class_Id, Parent_Id) :-

  (   objects:clause(copy(Class_Id, _, _), _)
  ->  true
  ;   assertz(objects:
             (copy(Class_Id, From, To) :- copy(Parent_Id, From, To))
      )
  ).


