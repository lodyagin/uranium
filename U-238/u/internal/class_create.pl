%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(class_create,
          [class_create/3,
           class_create/4,
           class_rebase/3
          ]).

:- use_module(u(internal/objects_i)).
:- use_module(u(internal/check_arg)).

check_class_create(Class, Parent, Fields, Ctx) :-

  (  (var(Class) ; var(Parent); var(Fields))
  -> throw(error(instantiation_error, Ctx))
  ;  true ),
  check_class_arg(Class, Ctx),
  check_class_arg(Parent, Ctx),
  (  is_list(Fields) -> true
  ;  throw(error(type_error(list, Fields), Ctx))
  ),
  (  class_primary_id(Parent, _)
  -> true
  ;  throw(error(existence_error(uranium_class, Parent), Ctx))
  ).

%
% class_create(+Class, +Parent, +Add_Fields)
%
% Assert the new Class definition into the objects module
%

class_create(Class, Parent, Fields) :-

   Ctx = context(class_create/3, _),
   check_class_create(Class, Parent, Fields, Ctx),
   class_primary_id(Parent, Parent_Id),
   assert_new_class(Class, Parent_Id, Fields, Ctx),
   get_key(Parent_Id, Parent_Key),
   class_primary_id(Class, Class_Id),
   assert_parent_key(Class_Id, Parent_Id),
   assert_copy(Class_Id, Parent_Id).
  
%
% class_create(+Class, +Parent, +Add_Fields, +Key)
%
% Assert the new Class definition into the objects module,
% set a (compound) key to Key
%

class_create(Class, Parent, Fields, New_Key) :-

   Ctx = context(class_create/4, _),
   check_class_create(Class, Parent, Fields, Ctx),
   (  var(New_Key)
   -> throw(error(instantiation_error, Ctx))
   ;  \+ is_list(New_Key)
   -> throw(error(type_error(list, New_Key), Ctx))
   ;  \+ is_set(New_Key)  
   -> throw(error(domain_error(no_duplicates, New_Key), Ctx))
   ;  % check the New_Key contents
      fields_names_types(New_Key, New_Key_Simpl, _),
      list_to_ord_set(New_Key_Simpl, New_Key_Set)
   ),
   class_primary_id(Parent, Parent_Id),
   assert_new_class(Class, Parent_Id, Fields, Ctx),
   get_key(Parent_Id, Parent_Key_Set),
   ord_union(Parent_Key_Set, New_Key_Set, Key_Set),
   length(Parent_Key_Set, Parent_Key_Length),
   length(Key_Set, Key_Length),
   (  Key_Length > Parent_Key_Length
   -> class_primary_id(Class, Class_Id),
      assert_new_key(Class_Id, Key_Set)
   ;  assert_parent_key(Class_Id, Parent_Id)
   ),
   assert_copy(Class_Id, Parent_Id).


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

  assert_new_class_id(Class_Id, Parent_Id, Fields, Ctx).


assert_new_class_rebased(Class, Parent_Id, New_Fields, Class_Id,
                         Ctx) :-

   % generate new id
   gen_new_class_id(Class_Id),
   objects:assertz(class_id(Class_Id, false, Class)),
   % NB arg2 = false means rebased class

   assert_new_class_id(Class_Id, Parent_Id, New_Fields, Ctx).


assert_class_fields(Class_Id, Top_Fields, Arg0, Arg) :-

   assert_inherited_fields(Class_Id, Class_Id, Arg0, Arg1),
   assert_class_fields2(Class_Id, Class_Id, Top_Fields, Arg1, Arg).

% reset Arg to 2 (always 1 holds class id)
assert_inherited_fields(_, 0, _, 2) :- !.

assert_inherited_fields(Class_Id, Ref_Class_Id, Arg0, Arg) :-

   % process parent's fields
   objects:parent(Ref_Class_Id, Parent_Id), !,
   assert_inherited_fields(Class_Id, Parent_Id, Arg0, Arg1),

   class_fields(Parent_Id, true, _, New_Fields),
   assert_class_fields2(Class_Id, Parent_Id, New_Fields,
                        Arg1, Arg).
   

assert_class_fields2(_, _, [], Arg, Arg) :- !. 


assert_class_fields2(Class_Id, Native_Id,
                    [Field_Name:Field_Type|FT], Arg0, Arg) :- !,

   (  Class_Id =\= Native_Id
   -> % re-assert inherited field with new class id
      objects:clause(
         field(Native_Id, Field_Name, Obj, Value, Field_Type,
            true, %true marks fields introduced by Native_Id
            Is_Eval), 
         Body),
      (  Is_Eval \== true
      -> 
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false, false) :- arg(Arg0, Obj, Value))
         ),
         Arg1 is Arg0 + 1
      ;  % eval fields not depends on Arg
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false, true) :- Body)
         ),
         Arg1 = Arg0
      )
   ;  % assert new fields
      % NB new evaluated fields are not in this list
      objects:assertz(
        (field(Class_Id, Field_Name, Obj, Value, Field_Type,
            true, false) :- arg(Arg0, Obj, Value))
      ),
      Arg1 is Arg0 + 1
   ),

   assert_class_fields2(Class_Id, Native_Id, FT, Arg1, Arg).

assert_class_fields2(Class_Id, Native_Id,
                    [Field_Name|FT], Arg0, Arg) :-

   assert_class_fields2(Class_Id, Native_Id,
                        [Field_Name:_|FT], Arg0, Arg).

                             
assert_new_key(_, []) :- !.

assert_new_key(Class_Id, Keys) :-

  assertz(objects:key(Class_Id, Class_Id, Keys)).

assert_parent_key(Class_Id, Parent_Id) :-

  (  objects:key(Parent_Id, _, Key)
  -> assertz(objects:key(Class_Id, Parent_Id, Key))
  ;  true ).


%
% Inherit copy from parent if not defined for Class
%

assert_copy(Class_Id, Parent_Id) :-

   class_id(Class_Id, Class),
   (   objects:clause(copy(Parent_Id, Class, From, To), Body)
   ->  objects:assertz(
         (copy(Class_Id, Class, From, To) :- Body)
      )
   ;  true
   ).


class_rebase([], -1, false) :- !.

class_rebase([Class_Orig_Id|Parents], Class_New_Id, Rebase) :-
   
   class_rebase(Parents, Parent_Id, Rebase1),

   % check whether do rebase
   (  Rebase1 \== rebase
   -> (  objects:parent(Class_Orig_Id, Parent_Id)
      -> Rebase = false
      ;  Rebase = rebase )
   ;
      % parents already rebased, need rebase
      Rebase = rebase
   ),

   % rebase if needed
   (  Rebase == rebase
   -> class_new_fields(Class_Orig_Id, New_Fields),
      class_id(Class_Orig_Id, Class),
      assert_new_class_rebased(Class, Parent_Id, New_Fields,
                               Class_New_Id, _)
   ;  % the same class is sufficient
      Class_New_Id = Class_Orig_Id
   ).

assert_new_class_id(Class_Id, Parent_Id, New_Fields, Ctx) :-

  % assert the parent relation
   objects:assertz(parent(Class_Id, Parent_Id)),

   assert_class_fields(Class_Id, New_Fields, _, Next_Arg),
   Arity is Next_Arg - 1,

   check_field_names_db(Class_Id, All_Field_Names, Ctx),

   !,
   % if all ok make final asserts
   class_noneval_new_fields_db(Class_Id, New_Field_Names),
   objects:assertz(fields(Class_Id, All_Field_Names,
                          New_Field_Names)),
   objects:assertz(arity(Class_Id, Arity)).


% All_Field_Names_Set - ordset of all noneval fields

check_field_names_db(Class_Id, All_Field_Names_Set, Ctx) :-

  % check no repeats in the class field names
  (  bagof(Field_Name,
          T1^T2^T3^T4^T5^(objects:clause(
            field(Class_Id, Field_Name, T1, T2, T3, T4, false),
            T5)),
          All_Field_Names)
  -> true
  ;  All_Field_Names = []
  ),
  (  \+ is_set(All_Field_Names)
  -> % delete incorrect definitions
     % FIXME remove class cleanup to catch
     objects:retractall(field(Class_Id, _, _, _, _, _, _)),
     throw(error(duplicate_field(All_Field_Names), Ctx))
  ;
     list_to_ord_set(All_Field_Names, All_Field_Names_Set)
  ).

% retrieve only fields appeared in Class_Id

class_noneval_new_fields_db(Class_Id, New_Field_Set) :-

   (  bagof(Field_Name,
           T1^T2^T3^T4^(objects:clause(
             field(Class_Id, Field_Name, T1, T2, T3, true, false),
                   T4)),
           Field_Names)
   -> list_to_ord_set(Field_Names, New_Field_Set)
   ;  New_Field_Set = []
   ).



