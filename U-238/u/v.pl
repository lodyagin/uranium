% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
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


:- module(v,
          [
           class_create/3,     % +Class, +Parent, +Add_Fields
           class_create/4,     % +Class, +Parent, +Add_Fields, +Key
           %class_descendant/2, % +Class, ?Descendant
           class_exists/1,     % ?Class
           class_fields_new/2,
           class_fields/2,     % +Class, -Fields (ordset)
           %class_field_type/3,
           class_name/1,       % ?Class
           class_parent/2,
           class_parents/2,
           %class_same_or_descendant/2, % +Class, ?Descendant
           eval_obj_expr/2,

           named_arg/3,
           named_arg/4,
           named_args_unify/3,
           named_args_weak_unify/3,
           %named_args_unify/5,

           obj_construct/4,
           obj_construct_weak/4,
           %obj_copy/2,         % +From, -To
           %obj_copy/3,         % +Field_List, +From, -To
           obj_is_descendant/2, % +Descendant, ?Class
           obj_same_or_descendant/2, % +Descendant, ?Class
           obj_diff/3,          % +Obj1, +Obj2, -Diff_List
           obj_diff_print/1,
           obj_diff_print/2,
           obj_downcast/2,     % +Parent, -Descendant
           obj_downcast/3,     % +Parent, +Class_To, -Descendant
           obj_field/3,        % +Obj, ?Field, ?Value
           obj_field/4,        % +Obj, +Weak, ?Field, ?Value
           obj_key/2,          % +Object, -Key
           obj_key_value/2,    % +Object, -Key_Value
           obj_list/2,         % +Object, -List
           obj_parents/2,      % +Object, -Class_Names_List
           obj_parents/3,      % +Obj0, +Class_Names_List, -Obj
           obj_rebase/3,       % ?Rebase_Rule, @Object0, -Object
           obj_reinterpret/2,  % +From, -To
           obj_rewrite/5,      % +Object0, +Fields, ?Old_Vals,
                               % +New_Vals, -Object
           obj_rewrite/6,      % +Object0, +Weak, +Fields, ?Old_Vals,
                               % +New_Vals, -Object

           obj_set_field/3,    % +Object, +Field, +Value
           obj_sort_parents/3, % +Obj0, +Class_Order, -Obj
           obj_reset_fields/3, % +[Field|...], +Obj_In, -Obj_Out
           obj_reset_fields/4, % +[Field|...], +Obj_In, -Obj_Out, Is_Succ
           obj_reset_fields_weak/3, % +[Field|...], +Obj_In, -Obj_Out
           obj_reset_fields_weak/4, % +[Field|...], +Obj_In, -Obj_Out, I
           obj_merge/4,
           obj_pretty_print/1,
           obj_pretty_print/2,
           obj_unify/3
           ]).

/** <module> Uranium object system.

  Uranium object is a compound term. See *|Uranium Book|* for the
  usage.

  ---++ Declaring objects
  @tbd

  ---+++ Keymaster
  _Keymaster_ is a class which introduces new key.
  @tbd

  ---++ weak, fail, throw matching of fields
  @tbd
  
*/

:- multifile prolog:message/3.

:- use_module(u(internal/objects)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(internal/class_create)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(u(ur_lists)).
:- use_module(u(logging)).

:- reexport(u(internal/objects_i),
            [
             u_class/1,
             u_object/1
            ]).

:- reexport(u(internal/object_module),
            [ reload_all_classes/0]).

% This is standard Weak arg values used in this module
std_weak_arg_values([[throw, throws, strict, s],
                     [unbound, weak, w],
                     [fail, false, f]
                    ]).

%
% class_create(+Class, +Parent, +Add_Fields)
%
% Assert the new Class definition into the objects module
%
% Add_Fields - (non-eval, native) fields from the class definition
%

class_create(Class, Parent, Fields) :-

   Ctx = context(class_create/3, _),
   class_create_cmn(Class, Parent, Fields, _, _, Ctx).

%
% class_create(+Class, +Parent, +Add_Fields, +Key)
%
% Assert the new Class definition into the objects module,
% set a (compound) key to Key
%

class_create(Class, Parent, Fields, New_Key) :-

   Ctx = context(class_create/4, _),
   class_create_cmn(Class, Parent, Fields, New_Key, _, Ctx).


class_exists(Class) :-

   Ctx = context(class_exists/1, _),
   check_inst(Class, Ctx),
   check_class_arg(Class, Ctx),
   class_primary_id(Class, _).

% obj_field(+Obj, ?Field_Name, ?Value)

obj_field(Obj, Field_Name, Value) :-

   Ctx = context(obj_field/3, _),

   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   (  var(Field_Name) -> true
   ;  check_field_name(Field_Name, Ctx)
   ),

   obj_field_int(Class_Id, Field_Name, throw, Obj, Value, _,
                 Ctx).

% obj_field(+Obj, +Weak, ?Field_Name, ?Value)

obj_field(Obj, Weak, Field_Name, Value) :-

   Ctx = context(obj_field/4, _),

   check_inst(Obj, Ctx),
   std_weak_arg_values(LOL),
   decode_arg(LOL, Weak, Weak1, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   (  var(Field_Name) -> true
   ;  check_field_name(Field_Name, Ctx)
   ),

   obj_field_int(Class_Id, Field_Name, Weak1, Obj, Value, _, Ctx).


named_arg(Obj, Field, Value) :-

  obj_field(Obj, Field, Value).


% named_arg(+Term, +Field_Name, ?Value, -Type)

named_arg(Term, Field_Name, Value, Type) :-

   Ctx = context(named_arg/4, _),
   check_inst(Term, Ctx), check_object_arg(Term, Ctx, Class_Id),
   (  var(Field_Name) -> true
   ;  check_field_name(Field_Name, Ctx)
   ),

   obj_field_int(Class_Id, Field_Name, throw, Term, Value, Type,
                 Ctx).

obj_unify(Term, Field_List, Value_List) :-

   named_args_unify(Term, Field_List, Value_List).

% named_args_unify(+Term, +Field_List, ?Value_List)

named_args_unify(Term, Field_List, Value_List) :-

   named_args_unify2(Term, Field_List, Value_List,
                     strict, context(named_args_unify/3, _)).

% weak means do not fail on unexisting fields

named_args_weak_unify(Term, Field_List, Value_List) :-

   named_args_unify2(Term, Field_List, Value_List,
                     weak, context(named_args_weak_unify/3, _)).

named_args_unify2(Term, Field_List, Value_List, Weak, Ctx) :-

   (  (var(Term) ; var(Field_List))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_fields_arg(Field_List, Ctx),

   (  var(Value_List) -> true
   ;  check_values_arg(Field_List, Value_List, Ctx)
   ),
   check_object_arg(Term, Ctx, Class_Id),

   std_weak_arg_values(LOL),
   decode_arg(LOL, Weak, Weak1, Ctx),

   obj_unify_int(Class_Id, Field_List, Weak1, Term, Value_List,
                 Ctx).


/*
named_args_unify(DB_Key,
       )          Functor,
                 [Field_Name | FN_Tail],
                 [Value | V_Tail],
                 w(Term_Ref, Term)
                ) :- !,

   named_arg_unify(DB_Key, Functor, Field_Name, Value,
                   w(Term_Ref, Term)),
   named_args_unify2(Term, FN_Tail, V_Tail).

named_args_unify(DB_Key,
                 Functor,
                 Field_Names,
                 Field_Values,
                 Term
                 ) :-

  named_args_unify(DB_Key,
                   Functor, Field_Names, Field_Values,
                   w(_, Term)).
*/

%
% Set the field as unbound
%

obj_reset_fields(Fields_List, Object_In, Object_Out) :-

   Ctx = context(obj_reset_fields/3, _),
   obj_reset_fields2(Fields_List, Object_In, Object_Out,
                        strict, Ctx).

%
% this form is for using in db_iterate_replace
%
obj_reset_fields(Fields_List, Object0, Object, true) :-

   Ctx = context(obj_reset_fields/4, _),
   obj_reset_fields2(Fields_List, Object0, Object, strict,
                        Ctx).

obj_reset_fields_weak(Fields_List, Object0, Object) :-

   Ctx = context(obj_reset_fields_weak/3, _),
   obj_reset_fields2(Fields_List, Object0, Object, weak, Ctx).

%
% this form is for using in db_iterate_replace
%
obj_reset_fields_weak(Fields_List, Object0, Object, true) :-

   Ctx = context(obj_reset_fields_weak/4, _),
   obj_reset_fields2(Fields_List, Object0, Object, weak, Ctx).


obj_reset_fields2(Fields_List, Object0, Object, Weak, Ctx) :-

   check_inst(Fields_List, Ctx),
   check_inst(Object0, Ctx),
   check_list_fast_arg(Fields_List, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),
   decode_arg([[weak], [fail, strict]], Weak, Weak1, Ctx),

   obj_reset_fields_int(Class_Id, Fields_List, Object0, Object,
                        Weak1, Ctx).


%
% Сборка экземпляра класса с установкой только заданных полей
%
% obj_construct(+Class, +Field_Names, ?Field_Values, -Object)
%

obj_construct(Class, Field_Names, Field_Values, Object) :-

   obj_construct2(Class, Field_Names, Field_Values, throw, Object).

%
% 'Weak' версия игнорирует поля в Field_Names, не присутствующие
% в объекте (и соответствующие значения)
%

obj_construct_weak(Class, Field_Names, Field_Values, Object) :-

   obj_construct2(Class, Field_Names, Field_Values, weak, Object).


obj_construct2(Class, Field_Names, Field_Values, Weak, Object) :-

   (  Weak \== weak
   -> Ctx = context(obj_construct/4, _)
   ;  Ctx = context(obj_construct_weak/4, _) ),

   (  (var(Class); var(Field_Names))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),

   check_existing_class_arg(Class, Ctx, Class_Id),
   check_fields_arg(Field_Names, Ctx),

   (  var(Field_Values)
   -> true
   ;  check_values_arg(Field_Names, Field_Values, Ctx)
   ),

   obj_construct_int(Class_Id, Field_Names, Weak, Field_Values,
                     Object).
%
% obj_downcast(+Parent, -Descendant).
%

obj_downcast(Parent, Descendant) :-

   Ctx = context(obj_downcast/2, _),
   check_inst(Parent, Ctx),
   check_object_arg(Parent, Ctx, _),
   obj_auto_downcast_int(Parent, Descendant, Ctx).


obj_auto_downcast_int(Parent, Descendant, Ctx) :-

   functor(Parent, Parent_Class, _),
   class_primary_id(Parent_Class, Parent_Class_Id),
   (  obj_field(Parent, class, Class),
      % check the result of the user-defined predicate
      nonvar(Class),
      u_class(Class),
      class_primary_id(Class, To_Class_Id)
   ->
      (  Parent_Class_Id =\= To_Class_Id
      -> obj_downcast_int(Parent_Class_Id, To_Class_Id, downcast,
                          Parent, Descendant1, Ctx),
         % recursion till no cast
         obj_auto_downcast_int(Descendant1, Descendant, Ctx)
      ;
         % the same class downcast
         Parent = Descendant
      )
   ;
      print_message(warning, bad_eval_result(Parent, class)),
      Parent = Descendant
   ).


% obj_downcast(+From, +To_Class, -To)
%
% downcast to Class
%

obj_downcast(From, To_Class, To) :-

   % TODO check To_Class below From_Class
   Ctx = context(obj_downcast/3, _),
   (  (var(From); var(To_Class))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_existing_class_arg(To_Class, Ctx),
   check_object_arg(From, Ctx, From_Class_Id),
   functor(From, From_Class, _),

   (  From_Class == To_Class
   ->
      To = From
   ;
      class_primary_id(To_Class, To_Class_Id),
      obj_downcast_int(From_Class_Id, To_Class_Id, downcast,
                       From, To, Ctx)
   ).


obj_downcast_int(From_Class_Id, To_Class_Id, Mode, From, To,
                 Ctx) :-

   From_Class_Id \== To_Class_Id,

   %  Check the downcast condition
   class_id(From_Class_Id, From_Class),
   (  same_or_descendant(To_Class_Id, true, From_Class)
   -> true
   ;  class_id(To_Class_Id, To_Class),
      throw(not_downcast(From_Class, To_Class))
   ),

   % Construct To object with all fields unbounded
   % (it will allow fields rewriting in user-defined downcast
   % or reinterpret).
   obj_construct_int(To_Class_Id, [], strict, [], To),

   (  Mode == downcast
   -> downcast_fill_values(From_Class_Id, To_Class_Id, From, To)
   ;  reinterpret_fill_values(From_Class_Id, To_Class_Id, From,
                              To)
   ),

   % Unify fields which are still unbounded
   unbounded_fields(To, Unbound_U),
   list_to_ord_set(Unbound_U, Unbound_Fields),
   obj_unify_int(From_Class_Id, Unbound_Fields, unbound, From,
                 Field_Values, Ctx),

   (  obj_unify_int(To_Class_Id, Unbound_Fields, fail, To,
                    Field_Values, Ctx)
   -> true
   ;
      % some fields are inter-bounded
      % it can be only the bug of user downcast

      throw(bad_downcast_impl(Mode, From, From_Class, To_Class,
                              To))
   ).


%
% Unify descendant's fields by object module downcast/4 rules
%
%TODO downcast to no direct descendant is not implemented

downcast_fill_values(Parent_Class_Id, Desc_Class_Id, Parent,
                     Desc) :-

   class_id(Parent_Class_Id, Parent_Class),
   class_id(Desc_Class_Id, Desc_Class),
   (  objects:clause(downcast(Parent_Class, Desc_Class, _, _), _)
   -> (  objects:downcast(Parent_Class, Desc_Class, Parent, Desc)
      -> true
      ;  throw(bad_downcast_impl(downcast,Parent, Parent_Class,
                                 Desc_Class, Desc))
      )
   ;  true % user downcast rule is not defined - it is ok
   ).

% unlike downcast_fill_values it is nondet

reinterpret_fill_values(Parent_Class_Id, Desc_Class_Id, Parent,
                     Desc) :-

   class_id(Parent_Class_Id, Parent_Class),
   class_id(Desc_Class_Id, Desc_Class),
   objects:reinterpret(Parent_Class, Desc_Class, Parent, Desc).


% obj_rebase(+Rebase_Rule, +Object0, -Object)

obj_rebase(Rebase_Rule, Object0, Object) :-

   Ctx = context(obj_rebase/3, _),
   check_inst(Object0, Ctx),
   check_rebase_rule(Rebase_Rule, Ctx, Old_Base, New_Base),
   
   check_object_arg(Object0, Ctx, Orig_Id),

   % Check the Old_Base and New_Base
   % convert it to the rebased base id if needed
   (  rebased_base(Orig_Id, Old_Base, Old_Base_Id) -> true
   ;  throw(error(old_base_is_invalid(Old_Base, Orig_Id), Ctx))
   ),

   (  rebased_base(Orig_Id, New_Base, New_Base_Id)
   -> New_Base_Is_Ancestor = true
   ;  class_primary_id(New_Base, New_Base_Id),
      New_Base_Is_Ancestor = false
   ),

   (  class_id(New_Base_Id, object_base_v)
   -> throw(error(cant_rebase_to_object_base_v, Ctx))
   ;  true ),

   % In some cases we do not need rebase
   (  New_Base_Is_Ancestor == true,
      same_or_descendant(New_Base_Id, _, Old_Base)
   ->
      true, % no need rebasing at all
      Object = Object0
   ;
      % find the common base class
      common_parent(Old_Base_Id, New_Base_Id, Cmn_Base_Id),

      % find the new parent line
      list_inheritance(New_Base_Id, New_Parents1),
      list_inheritance(Old_Base_Id, Orig_Id, [_|New_Parents2]),
      append(New_Parents1, New_Parents2, New_Parents_R),
      reverse(New_Parents_R, New_Parents3),

      class_rebase_int(New_Parents3, New_Parents, Rebase, Ctx),
      New_Parents = [Rebased_Id|_],
      (  Rebase == rebase -> true
      ;  throw(error(implementation_error(
          'Not rebasing case', []), Ctx))
      ),

      % find the fields to through out
      class_all_fields(Old_Base_Id, Old_Base_List),
      class_all_fields(Cmn_Base_Id, Cmn_Base_List),
      ord_subtract(Old_Base_List, Cmn_Base_List, Through_Out),
      % find the fields we will transfer to the new object
      class_all_fields(Orig_Id, Orig_List),
      ord_subtract(Orig_List, Through_Out, Transfer_Fields),
      obj_unify(Object0, Transfer_Fields, Transfer_Values),
      obj_construct_int(Rebased_Id, Transfer_Fields, strict,
                        Transfer_Values, Object)
   ).

% If Base is a same or ancestor return its id
% (it will be rebased if it is rebased already)
rebased_base(Id, Base, Id) :-

   class_id(Id, Base), !.

rebased_base(Id, Base, Base_Id) :-

   parent(Id, Parent_Id),
   rebased_base(Parent_Id, Base, Base_Id).




% obj_reinterpret(+From, -To) is nondet

obj_reinterpret(From, To) :-

   Ctx = context(obj_reiterpret/2, _),
   check_inst(From, Ctx),
   check_object_arg(From, Ctx, From_Class_Id),

   functor(From, From_Class, _),
   (  objects:clause(reinterpret(From_Class, To_Class, _, _), _),
      class_primary_id(To_Class, To_Class_Id),
      obj_downcast_int(From_Class_Id, To_Class_Id, reinterpret,
                       From, To, Ctx)
   ;
      To = From
   ).

% obj_rewrite(+Object0, +Fields, ?Old_Vals, +New_Vals, -Object)

obj_rewrite(Object0, Fields, Old_Vals, New_Vals, Object) :-

   Ctx = context(obj_rewrite/5, _),
   obj_rewrite_cmn(Object0, throw, Fields, Old_Vals, New_Vals,
                   Object, Ctx).

% obj_rewrite(+Object0, +Weak, +Fields, ?Old_Vals, +New_Vals,
%             -Object)

obj_rewrite(Object0, Weak, Fields, Old_Vals, New_Vals, Object) :-

   Ctx = context(obj_rewrite/6, _),
   obj_rewrite_cmn(Object0, Weak, Fields, Old_Vals, New_Vals,
                   Object, Ctx).

obj_rewrite_cmn(Object0, Weak, Fields, Old_Vals, New_Vals,
                Object, Ctx) :-

   check_inst(Object0, Ctx),
   check_inst(Fields, Ctx),
   check_inst(New_Vals, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),
   std_weak_arg_values(LOL),
   decode_arg(LOL, Weak, Weak1, Ctx),
   check_fields_arg(Fields, Ctx),
   (  var(Old_Vals) -> true
   ;  check_values_arg(Fields, Old_Vals, Ctx)
   ),
   check_values_arg(Fields, New_Vals, Ctx),

   obj_rewrite_int(Class_Id, Object0, Weak1, Fields, Old_Vals,
                   New_Vals, Object, Ctx).


% obj_set(+Object, +Field, +Value)
%
% Like obj_field/3 but do not unify Value, instead make a copy
% <NB> can be used for reset field

obj_set_field(Object, Field, Value) :-

   copy_term_nat(Value, Value1),
   obj_field(Object, Field, Value1).

% obj_sort_parents(+Obj0, +Class_Order, -Obj)
%
% Resort Obj0 parents according in th Class_Order
% (looking-up order), also removes duplicates.

obj_sort_parents(Obj0, Class_Order, Obj) :-

   Ctx = context(obj_sort_parents/3, _),
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, Class_Id),
   check_existing_class_list_arg(Class_Order, Ctx, _),
   
   obj_parents_int(Class_Id, Orig_Order),
   list_to_ord_set(Orig_Order, Orig_Order_Set),
   list_to_ord_set(Class_Order, Class_Order_Set),

   (  ord_subset(Orig_Order_Set, Class_Order_Set) -> true
   ;  throw(error(insufficient_class_order(Class_Order,
                                           Orig_Order), Ctx))
   ),
   ord_subtract(Class_Order_Set, Orig_Order_Set, Ignored_Set),
   subtract(Class_Order, Ignored_Set, New_Order),

   obj_parents_cmn(Obj0, New_Order, Obj, Ctx).

%
% Calculate expressions in an operator form
%

%eval_obj_expr(Sub_Expr ? Field, Value) :-
%	eval_obj_expr(Sub_Expr, Object),
%	named_arg(Object, Field, Value), !.

eval_obj_expr(Object0 / Field, Value) :- !,
    eval_obj_expr(Object0, Object),
    obj_field(Object, Field, Value).

eval_obj_expr(Value, Value).

%% class_descendant(+Class, ?Descendant) is nondet.
%
%  True if Descendant is a strict descendant of Class.
%  Does not count rebased classes
%
%  It is semidet if Descendant is bound.

% class_descendant(Class, Descendant) :-

%    Ctx = context(class_descendant/2, _),
%    check_inst(Class, Ctx),
%    check_existing_class_arg(Class, Ctx, Class_Id),

%    descendant_class(Class_Id, true, Descendant).


%% class_same_or_descendant(+Class, ?Descendant) is nondet.
%
%  True if Descendant = Class or it is a descendant of Class.
%  Does not count rebased classes
%
%  It is semidet if Descendant is bound.

% same_or_descendant(Class, Descendant) :-

%    Ctx = context(same_or_descendant/2, _),
%    check_inst(Class, Ctx),
%    check_existing_class_arg(Class, Ctx, Class_Id),

%    objects_i:same_or_descendant(Class_Id, true, Descendant).


%% class_name(?Class) is nondet.
%
% Class is a name of existing class.
% The primary usage is BT on classes.
% See u_class/1 for semidet check.

class_name(Class) :-

   objects:class_id(_, true, Class).


%% class_parent(?Class, ?Parent) is nondet.
%
% Ignore rebased classes.

class_parent(Class, Parent) :-

   nonvar(Class), nonvar(Parent), !,
   Ctx = context(class_parent/2, _),
   check_existing_class_arg(Class, Ctx),
   class_primary_id(Class, Class_Id),
   check_existing_class_arg(Parent, Ctx),
   class_primary_id(Parent, Parent_Id),
   parent(Class_Id, Parent_Id).

class_parent(Class, Parent) :-

   nonvar(Class), !,
   Ctx = context(class_parent/2, _),
   check_existing_class_arg(Class, Ctx),
   class_primary_id(Class, Class_Id),
   parent(Class_Id, Parent_Id),
   class_id(Parent_Id, Parent).

class_parent(Class, Parent) :-

   nonvar(Parent), !,
   Ctx = context(class_parent/2, _),
   check_existing_class_arg(Parent, Ctx),
   class_primary_id(Parent, Parent_Id),
   parent(Class_Id, Parent_Id),
   class_id(Class_Id, Class).

class_parent(Class, Parent) :-

   parent(Class_Id, Parent_Id),
   class_id(Class_Id, Class),
   class_id(Parent_Id, Parent).


%% class_parents(+Class, -Parents) is det.
%
% Return the normal (not rebased) parents list started
% with Class, for example
%
% ==
% ?- class_parents(citizen_v, P).
% P = [citizen_v, man_v, object_v, object_base_v].
% ==

class_parents(Class, Parents) :-

   Ctx = context(class_parents/2, _),
   check_inst(Class, Ctx),
   check_existing_class_arg(Class, Ctx, Class_Id),

   list_inheritance(Class_Id, Id_List_Rev),
   reverse(Id_List_Rev, Id_List),
   maplist(class_id, Id_List, Parents).


%% obj_is_descendant(+Descendant, ?Class) is nondet.
%
%  True if Descendant is a strict descendant of Class.
%  Count rebased classes.
%
%  It is semidet if Class is bound.

obj_is_descendant(Descendant, Class) :-

   Ctx = context(obj_is_descendant/2, _),
   check_inst(Descendant, Ctx),
   check_object_arg(Descendant, Ctx, Desc_Class_Id),
   descendant_class(Desc_Class_Id, _, Class).


% obj_same_or_descendant(+Descendant, ?Class) is nondet.
%
%  True if Descendant = Class or it is a descendant of Class.
%  Count rebased classes.
%
%  It is semidet if Class is bound.

obj_same_or_descendant(Descendant, Class) :-

   Ctx = context(obj_same_or_descendant/2, _),
   check_inst(Descendant, Ctx),
   check_object_arg(Descendant, Ctx, Desc_Class_Id),
   same_or_descendant(Desc_Class_Id, _, Class).


% class_new_fields(+Class, -Field_Names)
%
% Get list of fields introduced in Class

class_fields_new(Class, Field_Names) :-

   Ctx = context(class_fields_new/2, _),
   (  var(Class)
   -> throw(error(instantiation_error, Ctx))
   ;  check_existing_class_arg(Class, Ctx)
   ),
   class_primary_id(Class, Class_Id),
   class_new_fields(Class_Id, Field_Names).


% class_fields(+Class, -Field_Names)
%
% Get list of field names as ordset

class_fields(Class, Field_Names) :-

   Ctx = context(class_fields/2, _),
   (  var(Class)
   -> throw(error(instantiation_error, Ctx))
   ;  check_existing_class_arg(Class, Ctx)
   ),
   class_primary_id(Class, Class_Id),
   class_all_fields(Class_Id, Field_Names).

obj_pretty_print(Object) :-

  obj_pretty_print([lf(1)], Object).

obj_pretty_print(Options, Object) :-

   (  var(Object)
   -> throw(error(instantiation_error,
                  context(obj_pretty_print/2, _)))
   ;  check_object_arg(Object,
                       context(obj_pretty_print/2, _),
                       Class_Id)
   ),
   class_all_fields(Class_Id, Fields),
   class_id(Class_Id, Class),
   %open_log([lf(2, before)]),
   log_piece([Class, '('], Options),
   change_indent(Options, O2, 2),
   maplist(field_pretty_print(O2, Object), Fields),
   log_piece([')'], Options).
   %close_log([lf(2, after)]).

obj_diff_print(Diff_List) :-

   obj_diff_print([lf(1)], Diff_List).


obj_diff_print(Options, Diff_List_U) :-

   sort(Diff_List_U, Diff_List),
   log_piece('(', Options),
   change_indent(Options, O2, 2),
   maplist(one_diff_print(O2), Diff_List),
   log_piece(')', Options).


one_diff_print(Options, diff(Field, Before, After)) :-

   log_piece([Field, ':', Before, '->', After], Options).


field_pretty_print(Options, Object, Field) :-

  named_arg(Object, Field, Value, Type),
  (  ( var(Value) ; memberchk(hide_field(Field), Options))
  -> true
  ;  var(Type)
  -> Pretty_Value = Value
  ;  objects:current_predicate(pretty_print/4),
     objects:pretty_print(Type, atom(Pretty_Value), Value, Options)
  -> true
  ;  Pretty_Value = Value
  ),

  (  var(Pretty_Value)
  -> true
  ;  u_object(Pretty_Value)
  -> log_piece([Field, ':'], Options),
     change_indent(Options, O2, 2),
     log_piece([], Options),
     %exclude_lf(Options, O2),
     obj_pretty_print(O2, Pretty_Value)
     %log_piece([], Options)
  ;  log_piece([Field, ':', Pretty_Value], Options)
  ).


% C = A unify B

obj_merge(A, B, Class, C) :-

   % TODO db addr?
   obj_downcast(A, Class, A1),
   obj_downcast(B, Class, B1),
   A1 = B1,
   C = B1.


%
% obj_key(+Object, -Key)
%

obj_key(Object, Key) :-

  Ctx = context(obj_key/2, _),
  check_object_arg(Object, Ctx, Class_Id),
  get_key(Class_Id, Key).


%
% obj_key_value(+Object, -Key_Value)
%

obj_key_value(Object, Key_Value) :-

  Ctx = context(obj_key_value/2, _),
  check_object_arg(Object, Ctx, Class_Id),
  get_key(Class_Id, Key),
  obj_unify_int(Class_Id, Key, throw, Object, Key_Value, Ctx).


%% obj_list(+Object, -List) is semidet.
%
% Convert Object to the List [Field = Value, ...]
% It fails if Object is a free variable.

obj_list(Object, List) :-

   nonvar(Object), !,
   Ctx = context(obj_list/2, _),
   check_object_arg(Object, Ctx, Class_Id),
   class_all_fields(Class_Id, Fields),
   reverse(Fields, Fields_Rev),
   obj_list2(Fields_Rev, Object, [], List).

obj_list2([], _, List, List) :- !.

obj_list2([Field|Tail], Object, List0, List) :-

   obj_field(Object, Field, Value),
   (  nonvar(Value)
   -> List1 = [Field = Value|List0]
   ;  List1 = List0
   ),
   obj_list2(Tail, Object, List1, List).


%% obj_parents(+Object, -Class_Names_List) is det.
%
% Get the parents of Object in the looking-up order
% including the Object class.  i.e. Object = man_v ->
% Class_Names_List = [man_v, object_v, object_base_v]

obj_parents(Object, Class_Names_List) :-

   Ctx = context(obj_parents/2, _),
   check_inst(Object, Ctx),
   check_object_arg(Object, Ctx, Class_Id),

   obj_parents_int(Class_Id, Class_Names_List).

obj_parents_int(Class_Id, Class_Names_List) :-

   list_inheritance(Class_Id, Id_List_Rev),
   reverse(Id_List_Rev, Id_List),
   maplist(class_id, Id_List, Class_Names_List).


%% obj_parents(+Obj0, +Class_Names_List, -Obj)
%
% Rebase Obj0 such way that Class_Names_List is the new
% list of parents in Obj, for example:
%
% ==
% obj_construct(citizen_v, [], [], Man1),
% obj_parents(Man1, Parents1),
% obj_parents(Man1, [man_v, citizen_v, object_v,
%                    object_base_v], Man2),
% obj_parents(Man2, Parents2).
%
% Parents1 = [citizen_v, man_v, object_v, object_base_v],
% Parents2 = [man_v, citizen_v, object_v, object_base_v].
% ==

obj_parents(Obj0, Class_Names_List, Obj) :-

   Ctx = context(obj_parents/3, _),
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, _),
   obj_parents_cmn(Obj0, Class_Names_List, Obj, Ctx).

obj_parents_cmn(Obj0, Class_Names_List, Obj, Ctx) :-

   check_existing_class_list_arg(Class_Names_List, Ctx,
                                 Class_Ids_List),
   % TODO always check [..|object_v, object_base_v] ?

   class_rebase_int(Class_Ids_List, [New_Class_Id|_], _, Ctx),
   Class_Names_List = [New_Functor|_],

   Obj0 =.. [_, _|Obj0_T],
   Obj =.. [New_Functor, New_Class_Id|Obj0_T].

%
% obj_diff(+Obj1, +Obj2, -Diff_List)
% (skip eval fields)

obj_diff(Obj1, Obj2, Diff_List) :-

   Ctx = context(obj_diff/3, _),
   (  (var(Obj1); var(Obj2))
   -> throw(error(instantiation_error, Ctx))
   ;  check_object_arg(Obj1, Ctx, Class1_Id),
      check_object_arg(Obj2, Ctx, Class2_Id)
   ),

   class_all_fields(Class1_Id, Fields1),
   class_all_fields(Class2_Id, Fields2),

   ord_union(Fields1, Fields2, Fields_For_Diff),

   build_diff_list(Fields_For_Diff, Obj1, Obj2, [], Diff_List).


build_diff_list([], _, _, Diff, Diff) :- !.

build_diff_list([Field|Tail], Obj1, Obj2, Diff_In, Diff_Out) :-

  (obj_field(Obj1, fail, Field, V1) -> true ; V1 = _),
  (obj_field(Obj2, fail, Field, V2) -> true ; V2 = _),
  (V1 =@= V2 -> Diff_In = Diff2
   ; selectchk(diff(Field, V1, V2), Diff2, Diff_In)
   ),
  build_diff_list(Tail, Obj1, Obj2, Diff2, Diff_Out).


%
% Copy objects by class rules
%
% obj_copy(+From, -To)
%
/*
obj_copy(From, To) :-

   Ctx = context(obj_copy/2, _),
   (  var(From)
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_object_arg(From, Ctx, Class_Id),

   obj_copy_int(Class_Id, From, To).

%
% Copy objects by class rules, reset all fields
% not specified in Field_List
%
% obj_copy(+Field_List, +From, -To)
%

obj_copy(Field_List, From, To) :-

   Ctx = context(obj_copy/3, _),
   (  (var(Field_List); var(From))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_fields_arg(Field_List, Ctx),
   check_object_arg(From, Ctx, Class_Id),

   list_to_ord_set(Field_List, Copy_Fields),
   class_all_fields(Class_Id, Obj_Fields),
   ord_subtract(Obj_Fields, Copy_Fields, Reset_List),
   obj_reset_fields(Reset_List, From, Raw_Copy),

   obj_copy_int(Class_Id, Raw_Copy, To).


obj_copy_int(Class_Id, From, To) :-

   class_id(Class_Id, Class),
   (  objects:copy(Class_Id, Class, From, To)
   -> true
   ;  print_message(warning, undef_operation(copy, Class_Id))
   ).
*/


%class_field_type(Class, Field, Type) :-
%
%   class_arg_num(Class, Arg_Num, Field),
%   atom_concat(Class, '#t', Type_Functor),
%   objects:current_predicate(Type_Functor, Type_Term),
%   objects:Type_Term,
%   arg(Arg_Num, Type_Term, Type), !.


:- initialization clear_decode_arg.
