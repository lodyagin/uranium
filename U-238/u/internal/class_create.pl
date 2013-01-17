%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
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
          [class_create_cmn/6, % +Class, +Parent, +Fields,
                               % ?New_Key, -Class_Id, +Ctx

           class_rebase_int/4, % +Parents, -Parents_Ids, -Rebased,
                               % +Ctx
           obj_parents_int/4
          ]).

:- use_module(library(error)).
:- use_module(u(internal/objects)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/ur_debug)).


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

% class_create_cmn(+Class, +Parent, +Fields, ?New_Key,
%                  -Class_Id, +Ctx)
%

class_create_cmn(Class, Parent, Fields, New_Key, Class_Id, Ctx) :-

   check_class_create(Class, Parent, Fields, Ctx),
   (  nonvar(New_Key)
   ->
      check_inst(New_Key, Ctx),
      (  \+ is_list(New_Key)
      -> throw(error(type_error(list, New_Key), Ctx))
      ;  \+ is_set(New_Key)
      -> throw(error(domain_error(no_duplicates, New_Key), Ctx))
      ;  % check the New_Key contents
         fields_names_types(New_Key, New_Key_Simpl, _),
         list_to_ord_set(New_Key_Simpl, New_Key_Set)
      )
   ;  true
   ),
   class_create_int(Class, Parent, Fields, New_Key_Set, Class_Id,
                    Ctx).

class_create_int(Class, Parent, Fields, New_Key_Set, Class_Id,
                  Ctx) :-

   class_primary_id(Parent, Parent_Id),
   assert_new_class(Class, Parent_Id, Fields, Ctx),
   class_primary_id(Class, Class_Id),
   (  nonvar(New_Key_Set),
      get_key(Parent_Id, Parent_Key_Set),
      New_Key_Set \= Parent_Key_Set
   -> % <NB> when New_Key = [] reset the parent key
      assert_new_key(Class_Id, New_Key_Set)
   ;  assert_parent_key(Class_Id, Parent_Id)
   ),
   assert_copy(Class_Id, Parent_Id),
   assert_eval_fields(Class_Id).

assert_new_class(Class, Parent_Id, Fields0, Ctx) :-

  (  class_primary_id(Class, Class_Id)
  -> true % class id can be already created by object_module
  ;  gen_class_id(Class, Class_Id) ),

  % arity/2 normally only asserted by this module
  (  objects:arity(Class_Id, _)
  -> throw(error(class_exists(Class), Ctx))
  ;  true
  ),

  (  fields_names_types(Fields0, Field_Names0, _)
  -> true
  ;  throw(error(type_error(object_fields_and_types,Fields), Ctx))
  ),

  (  is_set(Field_Names0)
     % FIXME this check is only for non-eval fields
  -> true
  ;  Ctx = context(_, 'duplicates were found'),
     throw(error(domain_error(object_fields, Field_Names0), Ctx))
  ),

  (  \+ class_id(Class_Id, _)
  -> assertz(objects:class_id(Class_Id, true, Class)),
     debug(classes,
           'assertz(objects:class_id(~d, true, ~a))',
           [Class_Id, Class])
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
   class_fields(_:_, Parent_Id, _, false, Old_Fields),
   merge(Old_Fields, New_Fields, Fields),
   assert_class_fields2(Fields, 2, Next_Arg, Class_Id, Parent_Id),
   Arity is Next_Arg - 1.

assert_class_fields2([], Arg, Arg, _, _) :- !.

assert_class_fields2([Field_Name:Field_Type|FT], Arg0, Arg,
                        Class_Id, Parent_Id) :-

   % <NB> only non-eval fields

   (   objects:field_info(Parent_Id, Field_Name, _, _, _)
   ->  Native = false
   ;   Native = true
   ),

   objects:assertz(
                   field(Class_Id, Field_Name, Obj, Value)
                  :- arg(Arg0, Obj, Value)
                  ),
   debug(classes,
         'objects:assertz(field(~d, ~a, ~p, ~p) :- arg(~d, ~p, ~p))',
         [Class_Id, Field_Name, Obj, Value, Arg0, Obj,
          Value]),
   assertz(objects:field_info(Class_Id, Field_Name,
                              Field_Type, Native, false)),
   debug(classes,
         'assertz(objects:field_info(~d, ~a, ~p, ~a, false))',
         [Class_Id, Field_Name, Field_Type, Native]),
   Arg1 is Arg0 + 1,

   assert_class_fields2(FT, Arg1, Arg, Class_Id, Parent_Id), !.

assert_class_fields2([Field_Name|FT], Arg0, Arg, Class_Id,
                     Parent_Id) :-

   assert_class_fields2([Field_Name:_|FT], Arg0, Arg, Class_Id,
                        Parent_Id).


% assert_eval_fields(+Class_Id)
%
% For each (Class_Id, Field_Name) there are can be several
% eval_field preds. They should be asserted in order from more
% specific classes to more general ones.
%
% The evaluation oreder can differ for every Class_Id for the same
% Class because of rebasing

assert_eval_fields(Class_Id) :-

   class_id(Class_Id, Class),

   % TODO type for eval field
   (  setof(Name,
            Parent_Id^Type^Native^
            Obj^Value^Goal^(objects:eval_field(Class, Name,
                                               Obj, Value, Goal)
                           ;
                            parent(Class_Id, Parent_Id),
                            objects:field_info(Parent_Id, Name,
                                               Type, Native, true)
                           ),
            Eval_Fields)
   -> true
   ;  Eval_Fields = []
   ),

   maplist(assert_eval_field(Class_Id), Eval_Fields).

assert_eval_field(Class_Id, Field) :-

   eval_batch(Class_Id, Class_Id, Field, [], Batch),
   parent(Class_Id, Parent_Id),
   (  objects:field_info(Parent_Id, Field, _, _, _)
   -> Native = false
   ;  Native = true
   ),
   assert_eval_batch(Batch, Native),

   assertz_pred(classes, objects:field_info(Class_Id, Field, _,
                                            Native, true)).


eval_batch(0, _, _, L, L) :- !.  % object_base_v has no fields

eval_batch(Source_Class_Id, Class_Id, Field, L0, L) :-

   class_id(Source_Class_Id, Source_Class),
   atom_concat(Source_Class, '?', Name),
   Eval_Term =.. [Name, Object, Field, Value],
   Body = _:Eval_Term,

   findall((field(Class_Id, Field, Object, Value) :- Body),
           (
              Class_Id =:= Source_Class_Id,
              objects:eval_field(Source_Class, Field, Object,
                                 Value, Body),
              debug(classes, 'Find ~p',
                    [objects:eval_field(Source_Class, Field,
                                        Object, Value, Body)]),
              Native = true
           ;
              Class_Id =\= Source_Class_Id,
              objects:clause(field(Source_Class_Id, Field, Object,
                                   Value), Body),
              %debug(classes, 'Find ~p',
              %      [objects:clause(field(Source_Class_Id, Field, Object,
              %                     Value), Body)]),
              Native = false
           ),
           Batch),

   %retractall(objects:eval_field(Source_Class_Id, Field, _, _,
   %                              _)),
   append(L0, Batch, L1),
   %debug(classes, '~p', [append(L0, Batch, L1)]),

   parent(Source_Class_Id, Parent_Id),
   eval_batch(Parent_Id, Class_Id, Field, L1, L).


assert_eval_batch([], _) :- !.

assert_eval_batch([Field_Pred|T], Native) :-

   %Field_Pred = (field(Class_Id, Field, _, _) :- _),
   %(  objects:field_info(Class_Id, Field, _,
   assertz_pred(classes, objects:Field_Pred),
   assert_eval_batch(T, Native).

assert_new_key(_, []) :- !.

assert_new_key(Class_Id, Keys) :-

   must_be(positive_integer, Class_Id),
   assertz(objects:key(Class_Id, Class_Id, Keys)),
   debug(classes, '~p',
         [assertz(objects:key(Class_Id, Class_Id, Keys))]).

assert_parent_key(Class_Id, Parent_Id) :-

   must_be(positive_integer, Class_Id),
  (  objects:key(Parent_Id, _, Key)
  -> assertz(objects:key(Class_Id, Parent_Id, Key)),
     debug(classes, '~p',
           [assertz(objects:key(Class_Id, Parent_Id, Key))])
  ;  true ).


% assert_copy(+Class_Id, +Parent_Id)
%
% Inherit copy from parent if not defined for Class

assert_copy(Class_Id, Parent_Id) :-

   class_id(Class_Id, Class),
   %class_primary_id(Class, Primary_Id),

   (   objects:clause(copy(Class_Id, Class, From, To),
                      Body)
   ->
       true % it is already asserted (e.g. by
            % object_module)
   ;
   %     Class_Id \= Primary_Id,
   %     objects:clause(copy(Primary_Id, Class, From, To),
   %                    Body)
   % ->
   %     % Get the copy from a primary definition
   %     objects:assertz(
   %       (copy(Class_Id, Class, From, To) :- Body)
   %     ),
   %     debug(classes, '~p',
   %        objects:assertz(
   %           (copy(Class_Id, Class, From, To) :- Body)))
   % ;
       % Inherit a copy from parent
       objects:clause(copy(Parent_Id, _, From, To),
                      Body)
   ->
       objects:assertz(
         (copy(Class_Id, Class, From, To) :- Body)
       ),
       debug(classes, '~p',
          objects:assertz(
             (copy(Class_Id, Class, From, To) :- Body)))
   ;
       print_message(error,
                     class_definition_error(class_create,
                       assert_copy(Class_Id, Parent_Id)))
   ).



%% class_rebase_int(+Parents, -Parents_Ids, -New_Class, +Ctx) is semidet.
%
% Parents - a list of new parents names for this class (it
% is started from the nearest and ended with object_base_v
% (id = 0).
%
% It is det on existing class ids.
%
% @param Parents_Ids - new Ids for Parents.
% @param New_Class will be unified with true if new Class_Id
% is created and unbound in other case

class_rebase_int([object_base_v], [0], false, _) :- !.

class_rebase_int([Class|Parents],
                 [Class_New_Id|Parents_Ids],
                 New_Class, Ctx) :-

   % The recursion into parents
   class_rebase_int(Parents, Parents_Ids, New_Class_For_Parent, Ctx),
   Parents_Ids = [Parent_Id|_],

   % Check the rebased classes cache (by the class name)
   % It bounds New_Class or Class_New_Id (at least one)
   (  objects:rebased_class(Class, Parents, Class_New_Id)
   -> true
   ;
      % check whether do rebase
      (  New_Class_For_Parent == true
      ->
         % parents already rebased, need rebase
         New_Class = true
      ;
         (  % search existing Class descendant of Parent_Id
            class_id(Class_New_Id, Class),
            objects:parent_(Class_New_Id, Parent_Id)
         -> true
         ;  New_Class = true
         )
      ),

      class_primary_id(Class, Class_Prim_Id),
      % rebase if needed
      (  New_Class == true
      -> class_fields(_:_, Class_Prim_Id, true, false, New_Fields),
         assert_new_class_rebased(Class, Parent_Id, New_Fields,
                                  Class_New_Id, Ctx),

         get_key(Parent_Id, Parent_Key),
         get_key(Class_Prim_Id, Orig_Key),
         (  Parent_Key \= Orig_Key
         -> assert_new_key(Class_New_Id, Orig_Key)
         ;  assert_parent_key(Class_New_Id, Parent_Id)
         ),
         assert_copy(Class_New_Id, Parent_Id),
         assert_eval_fields(Class_New_Id)
      ;
         true % Class_New_Id should be already bound
      )
   ).



assert_new_class_id(Class_Id, Class_Name, Parent_Id, New_Fields,
                    Ctx) :-

   % assert the parent relation
   objects:assertz(parent_(Class_Id, Parent_Id)),
   debug(classes,
         'objects:assertz(parent_(~d, ~d))',
         [Class_Id, Parent_Id]),

   assert_class_fields(Class_Id, New_Fields, Arity),

   check_field_names_db(Class_Id, _, Ctx),

   !,
   % if all ok make final asserts
   %class_noneval_new_fields_db(Class_Id, New_Field_Names),
   %objects:assertz(fields(Class_Id, All_Field_Names,
   %                       New_Field_Names)),
   assertz(objects:arity(Class_Id, Arity)),
   debug(classes,
         'assertz(objects:arity(~d, ~d))',
         [Class_Id, Arity]),

   % any class is rebase of itself
   list_inheritance(Class_Id, Parents_List0),
   reverse(Parents_List0, [_|Parents_List]),
   (  objects:rebased_class(Class_Name, Parents_List,
                            Class_Id)
   -> true
   ;  assertz(objects:rebased_class(Class_Name, Parents_List,
                                    Class_Id)),
      debug(classes,
            'assertz(objects:rebased_class(~a, ~p, ~d))',
            [Class_Name, Parents_List, Class_Id])
   ).


% All_Field_Names_Set - ordset of all noneval fields

check_field_names_db(Class_Id, All_Field_Names_Set, Ctx) :-

  % check no repeats in the class field names
  % <NB> no objects:field_info checking
  (  bagof(Field_Name,
          T1^T2^(objects:clause(
            field(Class_Id, Field_Name, T1, false), T2)),
          All_Field_Names)
  -> true
  ;  All_Field_Names = []
  ),
  (  \+ is_set(All_Field_Names)
  -> % delete incorrect definitions
     % FIXME remove class cleanup to catch
     class_id(Class_Id, Class),
     objects:retractall(field(Class_Id, _, _, _)),
     objects:retractall(field_info(Class_Id, _, _, _, _)),
     objects:retractall(eval_field(Class, _, _, _, _)),
     throw(error(duplicate_field(All_Field_Names), Ctx))
  ;
     list_to_ord_set(All_Field_Names, All_Field_Names_Set)
  ).

% retrieve only fields appeared in Class_Id

class_noneval_new_fields_db(Class_Id, New_Field_Set) :-

   (  setof(Field_Name,
           Type^(field_info(Class_Id, Field_Name, Type,
                            true, false)),
           New_Field_Set)
   -> true
   ;  New_Field_Set = []
   ).


normalize_fields_def([], []) :- !.

normalize_fields_def([Name:Type|T1], [Name:Type|T2]) :- !,

  normalize_fields_def(T1, T2).

normalize_fields_def([Name|T1], [Name:_|T2]) :-

  normalize_fields_def(T1, T2).

obj_parents_int(Obj0, Parents, Obj, Ctx) :-

   class_rebase_int(Parents, [New_Id|_], _, Ctx),

   % find the fields we will transfer to the new object
   arg(1, Obj0, Orig_Id),
   class_all_fields(Orig_Id, Orig_Fields),
   class_all_fields(New_Id, New_Fields),
   ord_intersect(Orig_Fields, New_Fields, Transfer_Fields),
   obj_unify_int(Orig_Id, Transfer_Fields, throw, Obj0, Transfer_Values,
                 Ctx),
   obj_construct_int(New_Id, Transfer_Fields, strict,
                     Transfer_Values, Obj).



