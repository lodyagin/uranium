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

:- module(v,
          [
           class_descendant/2,
           class_exists/1,
           class_fields_new/2,
           class_fields/2,    %+Class, -Fields (ordset)
           %class_field_type/3,
           class_parent/2,
           eval_obj_expr/2,

           named_arg/3,
           named_arg/4,
           %named_arg_unify/5,
           named_args_unify/3,
           named_args_weak_unify/3,
           %named_args_unify/5,

           obj_construct/4,
           obj_construct_weak/4,
           %obj_copy/2,       % +From, -To
           %obj_copy/3,       % +Field_List, +From, -To
           obj_is_descendant/2,
           obj_diff/3,
           obj_diff_print/1,
           obj_diff_print/2,
           obj_downcast/2,   % +Parent, -Descendant
           obj_downcast/3,   % +Parent, +Class_To, -Descendant
           obj_field/3,
           obj_field_wf/3,
           obj_get_key/2,     % +Object, ?Key
           obj_rebase/3,     % ?Rebase_Rule, @Object0, -Object
           obj_reinterpret/2, % +From, -To
%           obj_reset_fields/3, % +[Field|...], +Obj_In, -Obj_Out
%           obj_reset_fields/4, % +[Field|...], +Obj_In, -Obj_Out, Is_Succ
%           obj_reset_fields_weak/3, % +[Field|...], +Obj_In, -Obj_Out
%           obj_reset_fields_weak/4, % +[Field|...], +Obj_In, -Obj_Out, I
           obj_merge/4,
           obj_pretty_print/1,
           obj_pretty_print/2,
           obj_unify/3,
           most_narrowed/3, %+Class1, +Class2, -Most_Narrowed_Class
%           class_arg_num/3, % +Class, ?Arg_Num, +Arg_Name
%           class_arg_num_weak/3,  % +Class, ?Arg_Num, +Arg_Name

           u_class/1,
           u_object/1
           ]).


:- reexport(u(internal/objects_i),
            [
             u_class/1,
             u_object/1
            ]).

:- reexport(u(internal/object_module),
            [ reload_all_classes/0]).

:- reexport(u(internal/class_create),
            [class_create/3,
             class_create/4]).


:- use_module(u(internal/objects_i)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/class_create)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
%:- use_module(u(vd)).
:- use_module(u(ur_lists)).
:- use_module(u(logging)).

%:- multifile db_recorded/3, db_erase/1, db_recordz/2.

/** <module> Uranium Objects



  */

class_exists(Class) :-

   Ctx = context(class_exists/1, _),
   check_inst(Class, Ctx),
   check_class_arg(Class, Ctx),
   class_primary_id(Class, _).

% obj_field(+Obj, +Field_Name, ?Value)

obj_field(Obj, Field_Name, Value) :-

   (  (var(Obj) ; var(Field_Name))
   -> throw(error(instantiation_error,
                  context(obj_field/3, _)))
   ;  \+ atom(Field_Name)
   -> throw(error(type_error(atom, Field_Name),
                  context(obj_field/3, _)))
   ;  check_object_arg(Obj, context(obj_field/3, _), Class_Id)
   ),
   obj_field_int(Class_Id, Field_Name, false, Obj, Value, _).


% obj_field_wf(+Obj, +Field_Name, ?Value)

% Weak version of obj_field/3 which fails the attribute
% on unexisting field instead of throwing the exception

obj_field_wf(Obj, Field_Name, Value) :-

   (  (var(Obj) ; var(Field_Name))
   -> throw(error(instantiation_error,
                  context(obj_field/3, _)))
   ;  \+ atom(Field_Name)
   -> throw(error(type_error(atom, Field_Name),
                  context(obj_field/3, _)))
   ;  check_object_arg(Obj, context(obj_field/3, _), Class_Id)
   ),
   obj_field_int(Class_Id, Field_Name, fail, Obj, Value, _).


named_arg(Obj, Field, Value) :-

  obj_field(Obj, Field, Value).


% named_arg(+Term, +Field_Name, ?Value, -Type)

named_arg(Term, Field_Name, Value, Type) :-

   (  (var(Term) ; var(Field_Name))
   -> throw(error(instantiation_error,
                  context(named_arg/4, _)))
   ;  \+ atom(Field_Name)
   -> throw(error(type_error(atom, Field_Name),
                  context(named_arg/4, _)))
   ;  check_object_arg(Term, context(named_arg/4, _), Class_Id)
   ),
   obj_field_int(Class_Id, Field_Name, strict, Term, Value, Type).

%
% Унификация с расширенной базой данных пролога по полю Field_Name
% и значению Value для тех фактов, которые созданы как классы
/*
named_arg_unify(DB_Key, Functor, Field_Name, Value, Term) :-

    ground(Field_Name),

    obj_field(Term, db_ref, Term_Ref),

    db_object_class(DB_Key, Functor), % unify with each class in db
    spec_term(Functor, Spec_Term),

    % find the position of the first object field Field_Name
    (arg(Field_Pos, Spec_Term, Field_Name) -> true; false),

    functor(Spec_Term, _, Arity),
    functor(Term, Functor, Arity),

    % Bound the field with the Value
    arg(Field_Pos, Term, Value),

    db_recorded(DB_Key, Term, Term_Ref).
*/

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
   obj_unify_int(Class_Id, Field_List, Weak, Term, Value_List).


/*
named_args_unify(DB_Key,
                 Functor,
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
/*
obj_reset_fields(Fields_List, Object_In, Object_Out) :-

  obj_reset_fields(Fields_List, Object_In, Object_Out, _).

%
% this form is for using in db_iterate_replace
%
obj_reset_fields(Fields_List, Object0, Object, true) :-

    functor(Object0, Class, _),
    maplist(class_arg_num(Class), Arg_Nums, Fields_List),
    duplicate_term(Object0, Object),
    length(Fields_List, N),
    length(Free_Var_List, N),
    maplist(setarg_tnv(Object), Arg_Nums, Free_Var_List), !.

obj_reset_fields_weak(Fields_List, Object0, Object) :-

  obj_reset_fields_weak(Fields_List, Object0, Object, _).


%
% this form is for using in db_iterate_replace
%
obj_reset_fields_weak(Fields_List, Object0, Object, true) :-

    functor(Object0, Class, _),
    maplist(class_arg_num_weak(Class), Arg_Nums0, Fields_List),
    delete(Arg_Nums0, 0, Arg_Nums),
    duplicate_term(Object0, Object),
    length(Arg_Nums, N),
    length(Free_Var_List, N),
    maplist(setarg_tnv(Object), Arg_Nums, Free_Var_List), !.
*/


%
% Сборка экземпляра класса с установкой только заданных полей
%
% obj_construct(+Class, +Field_Names, ?Field_Values, -Object)
%

obj_construct(Class, Field_Names, Field_Values, Object) :-

   obj_construct2(Class, Field_Names, Field_Values, false, Object).

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


obj_downcast_int(From_Class_Id, To_Class_Id, Mode, From, To, _) :-

   From_Class_Id \== To_Class_Id,

   %  Check the downcast condition
   (  same_or_descendant(From_Class_Id, true, To_Class_Id)
   -> true
   ;  class_id(From_Class_Id, From_Class),
      class_id(To_Class_Id, To_Class),
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
   obj_unify_int(From_Class_Id, Unbound_Fields, weak, From,
                 Field_Values),

   (  obj_unify_int(To_Class_Id, Unbound_Fields, strict, To,
                    Field_Values)
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
   check_rebase_rule(Rebase_Rule, Ctx),
   (  Rebase_Rule = '->'(Old_Base, New_Base)
   -> true
   ;  throw(error(type_error((->)/2, Rebase_Rule), Ctx))
   ),
   (  var(Object0)
   -> throw(error(instantiation_error, Ctx))
   ;  check_object_arg(Object0, Ctx, Orig_Id)
   ),

   Rebase_Rule = '->'(Old_Base, New_Base),
   class_primary_id(Old_Base, Old_Base_Id),
   class_primary_id(New_Base, New_Base_Id),

   % find the common base class
   common_parent(Old_Base_Id, New_Base_Id, Cmn_Base_Id),

   % find the new parent line
   list_inheritance(New_Base_Id, New_Parents1),
   list_inheritance(Old_Base_Id, Orig_Id, [_|New_Parents2]),
   append(New_Parents1, New_Parents2, New_Parents_R),
   reverse(New_Parents_R, New_Parents),

   class_rebase(New_Parents, Rebased_Id, Rebase),
   (  Rebase == rebase -> true
   ;  throw(implementation_error(
            ['class_rebase called for no actual rebasing case']))
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
                     Transfer_Values, Object).


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

%
% Вычисление выражений в операторной форме
%

%eval_obj_expr(Sub_Expr ? Field, Value) :-
%	eval_obj_expr(Sub_Expr, Object),
%	named_arg(Object, Field, Value), !.

eval_obj_expr(Object / Field, Value) :-
    obj_field(Object, Field, Value), !.

eval_obj_expr(Value, Value).

% class_descendant(+Class, ?Descendant)
%
% Does not count rebased classes

class_descendant(Class, Descendant) :-

   Ctx = context(class_descendant/2, _),
   check_inst(Class, Ctx),
   check_existing_class_arg(Class, Ctx),
   class_primary_id(Class, Class_Id),
   (  nonvar(Descendant)
   ->
      check_existing_class_arg(Descendant, Ctx),
      Class \== Descendant,
      class_primary_id(Descendant, Descendant_Id),
      same_or_descendant(Class_Id, true, Descendant_Id)
   ;
      same_or_descendant(Class_Id, true, Descendant_Id),
      Class_Id =\= Descendant_Id,
      class_id(Descendant_Id, Descendant)
   ).


% class_parent(?Class, ?Parent)
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


% obj_is_descendant(+Descendant, ?Class)

obj_is_descendant(Descendant, Class) :-

  Ctx = context(obj_is_descendant/2, _),
  check_inst(Descendant, Ctx),
  check_object_arg(Descendant, Ctx, Desc_Class_Id),

  class_primary_id(Class, Class_Id),
  Desc_Class_Id =\= Class_Id,
  same_or_descendant(Class_Id, _, Desc_Class_Id).


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
% If Class1 and Class2 has descending relation choose
% the most narrowed one.
%

most_narrowed(Class1, Class2, Most_Narrowed_Class) :-

  Class1 = Class2, Most_Narrowed_Class = Class1, !
  ;
  class_descendant(Class1, Class2),
  Most_Narrowed_Class = Class2, !
  ;
  class_descendant(Class2, Class1),
  Most_Narrowed_Class = Class1, !.

%
% obj_get_key(+Object, -Key)
%

obj_get_key(Object, Key) :-

  % TODO check args
  compound(Object), !,
  obj_class_id(Object, Class_Id),
  get_key(Class_Id, Key).


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

   build_diff_list(Obj1, Obj2, Fields_For_Diff, [], Diff_List).


build_diff_list(_, _, [], Diff, Diff) :- !.

build_diff_list(Obj1, Obj2, [Field|Tail], Diff_In, Diff_Out) :-

  (obj_field(Obj1, Field, V1) -> true ; V1 = _),
  (obj_field(Obj2, Field, V2) -> true ; V2 = _),
  (V1 =@= V2 -> Diff_In = Diff2
   ; selectchk(diff(Field, V1, V2), Diff2, Diff_In)
   ),
  build_diff_list(Obj1, Obj2, Tail, Diff2, Diff_Out).


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

