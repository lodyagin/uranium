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

:- module(ur_objects,
          [

           named_arg/3,
           named_arg/4,
           %named_arg_unify/5,
           named_args_unify/3,
           named_args_weak_unify/3,
           %named_args_unify/5,

           %class_fields/2,    %+Class, -Fields
           %class_field_type/3,
           eval_obj_expr/2,
           obj_construct/4,
           obj_construct_weak/4,
           obj_copy/2,       % +From, -To
           obj_copy/3,       % +Field_List, +From, -To
           obj_is_descendant/2,
           obj_diff/3,
           obj_diff_print/1,
           obj_diff_print/2,
           obj_downcast/2,   % +Parent, -Descendant
           obj_downcast/3,   % +Parent, +Class_To, -Descendant
           obj_field/3,
%           obj_rebase/3,     % ?Rebase_Rule, @Object0, -Object
%           obj_reset_fields/3, % +[Field|...], +Obj_In, -Obj_Out
%           obj_reset_fields/4, % +[Field|...], +Obj_In, -Obj_Out, Is_Succ
%           obj_reset_fields_weak/3, % +[Field|...], +Obj_In, -Obj_Out
%           obj_reset_fields_weak/4, % +[Field|...], +Obj_In, -Obj_Out, I
           obj_merge/4,
           obj_pretty_print/1,
           obj_pretty_print/2,
           obj_get_key/2,     % +Object, ?Key
           %get_key_value/2, % +Object, ?Key_Value
           most_narrowed/3, %+Class1, +Class2, -Most_Narrowed_Class
%           class_arg_num/3, % +Class, ?Arg_Num, +Arg_Name
%           class_arg_num_weak/3,  % +Class, ?Arg_Num, +Arg_Name

           u_class/1,
           u_object/1
           ]).


:- reexport(library(internal/objects_i),
            [
             u_class/1,
             u_object/1
            ]).

:- reexport(library(internal/object_module),
            [ reload_all_classes/0]).

:- use_module(library(internal/objects_i)).
:- use_module(library(internal/check_arg)).
:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(library(ur_recorded_db)).
:- use_module(library(ur_lists)).
:- use_module(library(ur_terms)).
:- use_module(logging/logging).


%:- multifile db_recorded/3, db_erase/1, db_recordz/2.

/** <module> Uranium Objects



  */

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

% NB evaluated fields can be also processed as `Weak' 

obj_field_int(Class_Id, Field_Name, Weak, Obj, Value, Type) :-

   (  objects:field(Class_Id, Field_Name, Obj, Value, Type, _)
   -> true
   ;  Weak == weak
   -> true
   ;  throw(no_object_field(Obj, Field_Name))
   ).


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


obj_unify_int(_, [], _, _, []) :- !.

obj_unify_int(Class_Id, [Field|FT], Weak, Term, [Value|VT]) :-

   obj_field_int(Class_Id, Field, Weak, Term, Value, _),
   obj_unify_int(Class_Id, FT, Weak, Term, VT).

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
   
   check_class_arg(Class, Ctx),
   check_fields_arg(Field_Names, Ctx),
   
   (  var(Field_Values)
   -> true
   ;  check_values_arg(Field_Names, Field_Values, Ctx)
   ),
   (  class_primary_id(Class, Class_Id) -> true
   ;  throw(error(existence_error(uranium_class, Class), Ctx))
   ),
   
   (  objects:arity(Class_Id, Arity) -> true
   ;  throw(class_system_bad_state(
            'no objects:arity/2 for class id ~d' - Class_Id),
            Ctx) ),
   functor(Object, Class, Arity),
   obj_class_id(Object, Class_Id),
   obj_unify_int(Class_Id, Field_Names, Weak, Object,
                 Field_Values).

%
% obj_downcast(+Parent, ?Descendant).
%

obj_downcast(Parent, Descendant) :-

  compound(Parent),
  obj_field(Parent, class, Class) ->
  obj_downcast(Parent, Class, Descendant)
  ;
  Parent = Descendant.


%
% downcast to Class
%

obj_downcast(Parent, Class, Descendant) :-

  compound(Parent),
  functor(Parent, Parent_Class, _),
  (
    Parent_Class = Class -> Parent = Descendant

  ;

    %class_ensure_created(Class),

    field_names_list(Class, Field_Names),
    Parent =.. [_|Parent_Field_Values],
    length(Field_Names, N_Flds_Desc),
    length(Parent_Field_Values, N_Flds_Parent),
    Diff is N_Flds_Desc - N_Flds_Parent,
    length(Add_Values, Diff),
    append(Parent_Field_Values, Add_Values, Field_Values),
    obj_construct(Class, Field_Names, Field_Values, Descendant), !,

    downcast_fill_values(Parent_Class, Class, Parent, Descendant)
  ).


%
% Unify descendants' fields by object module downcast/4 rules
%
% TODO: UT
%

downcast_fill_values(Parent_Class, Desc_Class, Parent, Desc) :-

  objects:clause(downcast(Parent_Class, Desc_Class, _, _), _) ->
  objects:downcast(Parent_Class, Desc_Class, Parent, Desc), !
   ;
  true, !.

%TODO downcast to no direct descendant is not implemented
%
%downcast_fill_values(Parent_Class, Desc_Class, Parent, Desc) :-
%
%  objects:downcast(Parent_Class, Child_Class, Parent, Child) ->
%  downcast_fill_values(Child_Class, Desc_Class, Child, Desc)
%  ;
%  true. % no objects:downcast defined

/*obj_rebase(Rebase_Rule, Object0, Object) :-

   (  Rebase_Rule = '->'(Old_Base, New_Base)
   -> true
   ;  throw(error(type_error((->)/2, Rebase_Rule),
                  context(obj_rebase/3, _))) 
   ),
   (  var(Object0)
   -> throw(error(instantiation_error,
                  context(obj_rebase/3, _)))
   ;  u_object(Object0)
   -> true
   ;  throw(error(type_error(uranium_object, Object0),
                  context(obj_rebase/3, _)))
   ).*/
      

  
%
% Вычисление выражений в операторной форме
%

%eval_obj_expr(Sub_Expr ? Field, Value) :-
%	eval_obj_expr(Sub_Expr, Object),
%	named_arg(Object, Field, Value), !.

eval_obj_expr(Object / Field, Value) :-
    obj_field(Object, Field, Value), !.

eval_obj_expr(Value, Value).

% obj_is_descendant(+Descendant, ?Class)

obj_is_descendant(Descendant, Class) :-

  check_inst(Descendant, context(obj_is_descendant/2, _)),
  check_object_arg(Descendant, context(obj_is_descendant/2, _),
                   _),
  (  nonvar(Class) -> check_class_arg(Class) ;  true ),

  class_prim_id(Class, _),
  class_descendant_i(Class, Descendant).

class_descendant_(Class, Descendant) :-

  class_parent(Descendant, Class).

class_descendant_(Class, Descendant) :-

  class_parent(Descendant0, Class),
  class_descendant(Descendant0, Descendant).

% Get list of field names

% FIXME Class_Id
%class_fields(Class, Field_Names) :-

  % TODO check args
%  class_id(Class_Id, Class),
%  class_fields_i(Class_Id, _, Fields),
%  fields_names_types(Fields, Field_Names, _, _).

obj_pretty_print(Object) :-

  obj_pretty_print([lf(1)], Object).

obj_pretty_print(Options, Object) :-

  functor(Object, Class, _),
  field_names_list(Class, Fields),
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
  (  var(Value)
  -> true
  ;  var(Type)
  -> Pretty_Value = Value
  ;  objects:current_predicate(pretty_print/4),
     objects:pretty_print(Type, atom(Pretty_Value), Value, Options)
  -> true
  ;  Pretty_Value = Value
  ),

  (  nonvar(Pretty_Value)
  -> log_piece([Field, ':', Pretty_Value], Options)
  ;  true
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


%get_key_value(Object, Key_Value) :-

%  functor(Object, Class, _),
%  get_key(Class, Key),
%  named_args_unify(Object, Key, Key_Value), !.

%
% obj_diff(+Obj1, +Obj2, -Diff_List)
%

obj_diff(Obj1, Obj2, Diff_List) :-

  compound(Obj1),
  compound(Obj2),

  functor(Obj1, Functor1, _),
  functor(Obj2, Functor2, _),

  class_fields(Functor1, Obj1_Fields),
  class_fields(Functor2, Obj2_Fields),

  % TODO track list of eval fields for each objects,
  % check for repeats in all field list
  %
%  findall(Field, 'object_v?'(object_v, Field, _), Object_V_Fields),
  Object_V_Fields = [],

  append(Obj1_Fields, Object_V_Fields, Fields1u),
  append(Obj2_Fields, Object_V_Fields, Fields2u),

  check_fields(Functor1, Fields1u),
  check_fields(Functor2, Fields2u),

  sort(Fields1u, Fields1),
  sort(Fields2u, Fields2),

  merge_set(Fields1, Fields2, Fields_For_Diff),

  build_diff_list(Obj1, Obj2, Fields_For_Diff, [], Diff_List).


build_diff_list(_, _, [], Diff, Diff) :- !.

build_diff_list(Obj1, Obj2, [Field|Tail], Diff_In, Diff_Out) :-

  (obj_field(Obj1, Field, V1) -> true ; V1 = _),
  (obj_field(Obj2, Field, V2) -> true ; V2 = _),
  (V1 =@= V2 -> Diff_In = Diff2
   ; selectchk(diff(Field, V1, V2), Diff2, Diff_In)
   ),
  build_diff_list(Obj1, Obj2, Tail, Diff2, Diff_Out).


check_fields(Class, Fields) :-

  is_set(Fields) -> true
  ; write_log(['Invalid fields list for', Class,
               ', there are repeated field names', Fields],
               [lf(2, before), lf(2)]).


%
% Copy objects by class rules
%
% obj_copy(+From, -To)
%
% FIXME
obj_copy(From, To) :-

   compound(From),
   functor(From, Class, _),
   objects:copy(Class, From, To).

%
% Copy objects by class rules, reset all fields
% not specified in Field_List
%
% obj_copy(+Field_List, +From, -To)
%

obj_copy(Field_List_U, From, To) :-

   ground(Field_List_U),
   is_set(Field_List_U),
   sort(Field_List_U, Field_List),
   compound(From),
   functor(From, Functor, _),

   class_fields(Functor, Obj_Fields_U),
   sort(Obj_Fields_U, Obj_Fields),
   ord_subtract(Obj_Fields, Field_List, Reset_List),
   obj_reset_fields(Reset_List, From, Raw_Copy),

   obj_copy(Raw_Copy, To).

%class_field_type(Class, Field, Type) :-
%
%   class_arg_num(Class, Arg_Num, Field),
%   atom_concat(Class, '#t', Type_Functor),
%   objects:current_predicate(Type_Functor, Type_Term),
%   objects:Type_Term,
%   arg(Arg_Num, Type_Term, Type), !.

