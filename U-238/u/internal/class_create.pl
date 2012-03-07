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

:- use_module(library(error)).
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
   class_primary_id(Class, Class_Id),
   assert_new_key(Class_Id, New_Key_Set),
   assert_copy(Class_Id, Parent_Id).


assert_new_class(Class, Parent_Id, Fields0, Ctx) :-

  (  class_primary_id(Class, Class_Id)
  -> true % class id can be already created by object_module
  ;  gen_class_id(Class, Class_Id) ),

  % arity/2 :- true is only asserted by this module
  (  objects:clause(arity(Class_Id, _), _)
  -> throw(class_exists(Class))
  ;  true
  ),

  (  fields_names_types(Fields0, Field_Names0, _)
  -> true
  ;  throw(error(type_error(object_fields_and_types,Fields), Ctx))
  ),
  (  is_set(Field_Names0) -> true
  ;  Ctx = context(_, 'duplicates were found'),
     throw(error(domain_error(object_fields, Field_Names0), Ctx))
  ),

  (  \+ class_id(Class_Id, _)
  -> objects:assertz(class_id(Class_Id, true, Class))
  ;  true ),

  list_to_ord_set(Fields0, Fields),
  assert_new_class_id(Class_Id, Class, Parent_Id, Fields, Ctx).


assert_new_class_rebased(Class, Parent_Id, New_Fields, Class_Id,
                         Ctx) :-

   % generate new id
   gen_new_class_id(Class_Id),
   objects:assertz(class_id(Class_Id, false, Class)),
   % NB arg2 = false means rebased class

   assert_new_class_id(Class_Id, Class, Parent_Id, New_Fields,
                       Ctx).


% assert_class_fields(+Class_Id, +New_Fields, -Arity)
assert_class_fields(Class_Id, New_Fields0, Arity) :-

   parent(Class_Id, Parent_Id),
   normalize_fields_def(New_Fields0, New_Fields),
   class_fields(_:_, Parent_Id, _, _, Old_Fields),
   merge(Old_Fields, New_Fields, Fields),
   assert_class_fields2(Fields, 2, Next_Arg, Class_Id, Parent_Id),
   Arity is Next_Arg - 1.

assert_class_fields2([], Arg, Arg, _, _) :- !.

assert_class_fields2([Field_Name:Field_Type|FT], Arg0, Arg,
                        Class_Id, Parent_Id) :-

   (  objects:clause(field(Parent_Id, Field_Name, Obj, Value,
                          Field_Type, _, Is_Eval), Body)
   -> % re-assert inherited field with new class id
      (  Is_Eval == false
      ->
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false, false) :- arg(Arg0, Obj, Value))
         ),
         Arg1 is Arg0 + 1
      ;
         objects:assertz(
           (field(Class_Id, Field_Name, Obj, Value, Field_Type,
                  false, true) :- Body)
         ),
         Arg1 = Arg0 % eval fields not depends on Arg
      )
   ;  % assert new fields
      % NB new evaluated fields are not in this list
      objects:assertz(
        (field(Class_Id, Field_Name, Obj, Value, Field_Type,
            true, false) :- arg(Arg0, Obj, Value))
      ),
      Arg1 is Arg0 + 1
   ),

   assert_class_fields2(FT, Arg1, Arg, Class_Id, Parent_Id), !.


assert_class_fields2([Field_Name|FT], Arg0, Arg, Class_Id,
                     Parent_Id) :-

   assert_class_fields2([Field_Name:_|FT], Arg0, Arg, Class_Id,
                        Parent_Id).


assert_new_key(_, []) :- !.

assert_new_key(Class_Id, Keys) :-

   must_be(positive_integer, Class_Id),
   assertz(objects:key(Class_Id, Class_Id, Keys)).

assert_parent_key(Class_Id, Parent_Id) :-

   must_be(positive_integer, Class_Id),
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


% class_rebase(+Parents, -Class_New_Id, -Rebased)
%
% Parents - a list of new parents ids for this class (it is
% started from nearest and ended with object_base_v (id = 0)
%

class_rebase([0], [0], false) :- !.

class_rebase([Class_Orig_Id|Parents0], [Class_New_Id|Parents],
             Rebase) :-

   % The recursion into parents
   class_rebase(Parents0, Parents, Rebase1),
   Parents = [Parent_Id|_],

   % Check the rebased classes cache (by the class name)
   class_id(Class_Orig_Id, Class_Name), % get Class_Name
   (  objects:rebased_class(Class_Name, Parents, Class_New_Id)
   ->
      Rebase = rebase
   ;
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
      -> class_fields(_:_, Class_Orig_Id, true, false, New_Fields),
         class_id(Class_Orig_Id, Class),
         assert_new_class_rebased(Class, Parent_Id, New_Fields,
                                  Class_New_Id, _),

         get_key(Class_Orig_Id, Orig_Key),
         assert_new_key(Class_New_Id, Orig_Key)
      
      ;  % the same class is sufficient
         Class_New_Id = Class_Orig_Id
      )
   ).

assert_new_class_id(Class_Id, Class_Name, Parent_Id, New_Fields,
                    Ctx) :-

   % assert the parent relation
   objects:assertz(parent(Class_Id, Parent_Id)),

   assert_class_fields(Class_Id, New_Fields, Arity),

   check_field_names_db(Class_Id, _, Ctx),

   !,
   % if all ok make final asserts
   %class_noneval_new_fields_db(Class_Id, New_Field_Names),
   %objects:assertz(fields(Class_Id, All_Field_Names,
   %                       New_Field_Names)),
   objects:assertz(arity(Class_Id, Arity)),

   % any class is rebase of itself
   list_inheritance(Class_Id, Parents_List0),
   reverse(Parents_List0, [_|Parents_List]),
   (  objects:rebased_class(Class_Name, Parents_List, Class_Id)
   -> true
   ;  assertz(objects:rebased_class(Class_Name, Parents_List,
                                    Class_Id))
   ).


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


normalize_fields_def([], []) :- !.

normalize_fields_def([Name:Type|T1], [Name:Type|T2]) :- !,

  normalize_fields_def(T1, T2).

normalize_fields_def([Name|T1], [Name:_|T2]) :- 

  normalize_fields_def(T1, T2).


