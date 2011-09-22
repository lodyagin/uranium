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
%  --------------------------------------------------------------
%   Description      : Data abstraction mechanismes.
%   Created On       : Apr 3 2009

:- module(ur_objects, 
          [
           named_arg/3,
           named_arg/4,
           named_arg_unify/5,
           named_arg_unify/6,
           named_args_unify/3,
           named_args_weak_unify/3,
           named_args_unify/5,
           named_args_unify/6,
           class_create/3,
           class_create/4,
           class_ensure_created/1,
           class_parent/2,
           class_descendant/2,
           class_fields/2,    %+Class, ?Fields
           class_field_type/3, 
           obj_construct/4,
           obj_construct/5,
           obj_diff/3,
           obj_diff_print/1,
           obj_diff_print/2,
           obj_downcast/2,   % +Parent, -Descendant
           obj_downcast/3,   % +Parent, +Class_To, -Descendant
           obj_copy/2,       % +From, -To
           obj_copy/3,       % +Field_List, +From, -To
           obj_field/3,
           obj_reset_fields/3, % +[Field|...], +Obj_In, -Obj_Out
           obj_reset_fields/4, % +[Field|...], +Obj_In, -Obj_Out, Is_Succ
           obj_reset_fields_weak/3, % +[Field|...], +Obj_In, -Obj_Out
           obj_reset_fields_weak/4, % +[Field|...], +Obj_In, -Obj_Out, I
           obj_merge/4,
           obj_pretty_print/1,
           obj_pretty_print/2,
           eval_obj_expr/2,
           get_key/2,     % +Class, ?Key | +Object, ?Key
           get_key_value/2, % +Object, ?Key_Value
           most_narrowed/3, %+Class1, +Class2, -Most_Narrowed_Class

           % low level 
           spec_term/2,     % +Class, -Spec_Term
           class_arg_num/3, % +Class, ?Arg_Num, +Arg_Name
           class_arg_num_weak/3  % +Class, ?Arg_Num, +Arg_Name
           ]).        

:- use_module(library(ur_lists)).
:- use_module(library(ur_terms)).
:- use_module(library(ur_recorded_db)).
:- use_module(logging/logging).

:- dynamic objects:key/2, objects:copy/3, 
           objects:typedef_flag/2, objects:pretty_print/4.

%:- multifile db_recorded/3, db_erase/1, db_recordz/2.


/** <module> Uranium Objects

  
  
  */

%
% Доступ к полю Term по имени
% (через описательный терм '...#').
%
% named_arg(+Term, +Field_Name, ?Value)
% (booked)

named_arg(Term, Field_Name, Value) :-

   named_arg(Term, Field_Name, Value, _).

% unbooked

named_arg(Term, Field_Name, Value, Type) :-

    functor(Term, Functor, Arity),
    atom_concat(Functor, '#', Spec_Functor),
    atom_concat(Spec_Functor, 't', Type_Functor),
    functor(Spec_Term, Spec_Functor, Arity),
    functor(Type_Term, Type_Functor, Arity),
    
    objects:Spec_Term,
    objects:Type_Term,

    (   arg(Field_Pos, Spec_Term, Field_Name)
    ->  arg(Field_Pos, Term, Value),
        arg(Field_Pos, Type_Term, Type)
    ;   atom_concat(Functor, '?', Method_Functor),
        objects:current_predicate(Method_Functor, _)
    ->  % Вызов метода
        call(objects:Method_Functor, Term, Field_Name, Value)
    ), 
    !.
         
    

%
% Унификация с расширенной базой данных пролога по полю Field_Name
% и значению Value для тех фактов, для
% которых есть описатель '...#'.
% booked

named_arg_unify(DB_Key, Functor, Field_Name, Value, Term) :-

  named_arg_unify(DB_Key, Functor, Field_Name, Value, Term, _).


named_arg_unify(DB_Key, Functor, Field_Name, Value, Term, Term_Ref) :-

    ground(Field_Name),

    db_object_class(DB_Key, Functor), % unify with each class in db
    spec_term(Functor, Spec_Term),
    (arg(Field_Pos, Spec_Term, Field_Name) -> true; false),   % нашли позицию поля по имени (первую!)

    functor(Spec_Term, _, Arity),
    functor(Term, Functor, Arity), 
    arg(Field_Pos, Term, Value),             % связали Value со значением
    db_recorded(DB_Key, Term, Term_Ref).


% spec_term(+Class, -Spec_Term)

spec_term(Class, Spec_Term) :-

    atom_concat(Class, '#', Spec_Functor), 
    objects:current_predicate(Spec_Functor, Spec_Term),
    objects:Spec_Term.

%
% Те же операции, но над списками.
%

% weak means do not fail on unexisting fields

named_args_weak_unify(_, [], []) :- !.

named_args_weak_unify(Term,
                 [Field_Name | FN_Tail],
                 [Value | V_Tail]
                ) :-

  (   obj_field(Term, Field_Name, V)
  ->  V = Value
  ;   true
  ),
  named_args_weak_unify(Term, FN_Tail, V_Tail).
 
 
named_args_unify(_, [], []) :- !.

named_args_unify(Term,
                 [Field_Name | FN_Tail],
                 [Value | V_Tail]
                ) :-

  obj_field(Term, Field_Name, Value),
  named_args_unify(Term, FN_Tail, V_Tail).

named_args_unify(DB_Key,
                 Functor,
                 Field_Names,
                 Field_Values,
                 Term
                 ) :-

  named_args_unify(DB_Key,
                   Functor, Field_Names, Field_Values, Term, _).

named_args_unify(DB_Key,
                 Functor,
                 [Field_Name | FN_Tail],
                 [Value | V_Tail],
                 Term,
                 Term_Ref
                ) :-

    named_arg_unify(DB_Key, Functor, Field_Name, Value, Term, Term_Ref),
    named_args_unify2(Term, FN_Tail, V_Tail).

named_args_unify2(_, [], []).

named_args_unify2(Term,
                  [Field_Name | FN_Tail],
                  [Value | V_Tail]
                 ) :-

    named_arg(Term, Field_Name, Value),
    named_args_unify2(Term, FN_Tail, V_Tail).

%
% Классы
%

obj_field(Object, Field_Name, Value) :-

    named_arg(Object, Field_Name, Value).

%
% Set the field as unbound
%

obj_reset_fields(Fields_List, Object_In, Object_Out) :-

  obj_reset_fields(Fields_List, Object_In, Object_Out, _).

  
obj_reset_fields(Fields_List, Object_In, Object_Out, true) :-

    functor(Object_In, Class, _),
    maplist(class_arg_num(Class), Arg_Nums, Fields_List),
    duplicate_term(Object_In, Object_Out),
    length(Fields_List, N),
    length(Free_Var_List, N),
    maplist(setarg_tnv(Object_Out), Arg_Nums, Free_Var_List), !.

obj_reset_fields_weak(Fields_List, Object_In, Object_Out) :-

  obj_reset_fields_weak(Fields_List, Object_In, Object_Out, _).

  
obj_reset_fields_weak(Fields_List, Object_In, Object_Out, true) :-

    functor(Object_In, Class, _),
    maplist(class_arg_num_weak(Class), Arg_Nums0, Fields_List),
    delete(Arg_Nums0, 0, Arg_Nums),
    duplicate_term(Object_In, Object_Out),
    length(Arg_Nums, N),
    length(Free_Var_List, N),
    maplist(setarg_tnv(Object_Out), Arg_Nums, Free_Var_List), !.


  
%
% Сборка экземпляра класса с установкой только заданных полей
%
% obj_construct(+Class, +Field_Names, +Field_Values, ?Term)
%

obj_construct(Class, Field_Names, Field_Values, Term) :-
  obj_construct(Class, Field_Names, Field_Values, Term, false).

% 
% 'Weak' версия игнорирует поля в Field_Names, не присутствующие
% в объекте (и соответствующие значения)
%

obj_construct(Class, Field_Names, Field_Values, Term, Weak) :-

    class_ensure_created(Class), 

    atom_concat(Class, '#', Spec_Class), 
    current_functor(Spec_Class, Arity),    % находим функтор-описатель
    !,
    functor(Spec_Term, Spec_Class, Arity), 
    objects:Spec_Term,

    functor(Term, Class, Arity), % создали объект с несвязанными полями

    % вычисляем номера полей
    (Weak = weak ->
     weak_maplist(arg_bac(Spec_Term), Field_Poses, Field_Names)
     ;
     maplist(arg_bac(Spec_Term), Field_Poses, Field_Names)
     ),

    % связываем поля с вычисленными номерами
    (Weak = weak ->
     weak_maplist(weak_arg_bac(Term), Field_Poses, Field_Values)
     ;
     maplist(arg_bac(Term), Field_Poses, Field_Values)
     ),
    !.

% todo it duplicates class_fields
field_names_list(Class, Field_Names) :-

   atom_concat(Class, '#', Meta_Pred_Functor),
   objects:current_predicate(Meta_Pred_Functor, Meta_Pred), !,
   objects:Meta_Pred,
   Meta_Pred =.. [Meta_Pred_Functor|Field_Names].

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

    class_ensure_created(Class), 

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
 
%
% Вычисление выражений в операторной форме
%

%eval_obj_expr(Sub_Expr ? Field, Value) :-
%	eval_obj_expr(Sub_Expr, Object),
%	named_arg(Object, Field, Value), !.

eval_obj_expr(Object / Field, Value) :-
    obj_field(Object, Field, Value), !.

eval_obj_expr(Value, Value).


%
% find_class_module(+Class, -Module_Path)
%
% If several modules (up to 5th level in dirs) match
% Class, then backtrace each.
%
% Class must end with '_v'.

find_class_module(Class, Module_Path) :-

  atom_concat(_, '_v', Class),
  atom_concat(Class, '.pl', Module_Name),
  expand_file_name('*_v.pl', L1),
  expand_file_name('*/*_v.pl', L2),
  expand_file_name('*/*/*_v.pl', L3),
  expand_file_name('*/*/*/*_v.pl', L4),
  expand_file_name('*/*/*/*/*_v.pl', L5),
  flatten([L1, L2, L3, L4, L5], List),
  member(Module_Path, List),
  atom_concat(Path_Head, Module_Name, Module_Path),
  (Path_Head = '' ; atom_concat(_, '/', Path_Head)).

%
% load_class_module(+Class, +Meta, +Module_Path)
%
% Load the class module,
% create classes, load meta predicates (_v?),
%
                             
load_class_module(Class, Meta, Module_Path) :-

  (flag(objects_module_created, 0, true) ->
   % ur_objects must be imported in the objects din module
   objects:use_module(library(ur_objects)) ; true),

  use_module(Module_Path), %% NB can't redefine

  % import *_v? predicates first (they are used by others)
  % NB use assert, not import to allow inherit _v? preds

  (Class:current_predicate(Head, Term),
   atom_concat(_, '_v?', Head),
   Class:clause(Term, Body),
   objects:assert((Term :- Body)),
   fail
   ;
   true),

  % import copy/3 predicates
  dynamic_import(Class, objects, copy),
 
  % process typedefs
  (  Class:current_predicate(typedef/2),
     Class:typedef(TD_Type, TD_List),

     % control for not repeating type definitions
     (  objects:typedef_flag(TD_Type, Some_Class)
     -> write_log(['Type', TD_Type, 
                   'is defined already in', Some_Class],
                  [lf(1)]),
        !, fail
     ;  objects:assert(typedef_flag(TD_Type, Class))
     ),

     % pretty_print
     (  memberchk(pretty_print - TD_PP_Head, TD_List)
     -> TD_PP_Pred =.. [TD_PP_Head, TD_Stream, TD_Value, TD_Opt],
        objects:assert((pretty_print(TD_Type, TD_Stream, TD_Value, TD_Opt)
                        :- Class:TD_PP_Pred))
     ;  true
     ),

     % postgres types
     (  memberchk(postgres - type(PG_Type, PG_Convert_Pred),
                  TD_List)

     -> db_pg:assert(pl_pg_type(TD_Type, PG_Type,
                                      Class:PG_Convert_Pred)
                          )
     ; true
     ),

     fail
  ;
     true
  ),
     

  % process new_class/3,4
       
  (
     (  current_predicate(Class:new_class/3),
        Class:new_class(Class_X, Parent_X, Add_Fields_X)
     ;  
        current_predicate(Class:new_class/4),
        Class:new_class(Class_X, Parent_X, Add_Fields_X, Key_X)
     ),

     % import class with its descendants

     atom_concat(Class_X, '#', Meta_X),
     \+ objects:current_predicate(Meta_X, _),

     (   nonvar(Key_X) 
     ->  class_create(Class_X, Parent_X, Add_Fields_X, Key_X)
     ;   class_create(Class_X, Parent_X, Add_Fields_X)
     ),
     fail
  ;
     (   objects:current_predicate(Meta, _)
     ->  true
     ;   write_log(['The module', Module_Path, 
                   'does not contain a definition for a class',
                   Class, '(no new_class/3 predicate was found)'])
     )
  ),

  % import downcast/4 predicates
  
  (current_predicate(Class:downcast/4) ->
   objects:import(Class:downcast/4) ; true).
                            
%
% import all predicates with head Functor as dynamic assert
%
dynamic_import(Module_From, Module_To, Functor) :-

  Module_From:current_predicate(Functor, Term),
  Module_From:clause(Term, Body),
  Module_To:assert((Term :- Body)),
  fail                  
  ;
  true. 
                           
%
% class_create(+Class, +Parent, +Add_Fields)
%
% Assert the new Class definition into the objects module
%                             

class_create(Class, object_base_v, Fields) :-

  !,
  atom(Class),
  maplist(field_name, Fields, Field_Names),
  (is_set(Field_Names) -> true;
   write_log(['Field duplicates found when creating object', 
             Class, 
             Field_Names]),
   fail
  ),
  assert_meta(Class, object_base_v, Fields).


class_create(Class, Parent, Add_Fields) :-

  atom(Class),
  class_ensure_created(Parent),
  atom_concat(Parent, '#', Parent_Meta),
  objects:current_predicate(Parent_Meta, Parent_Meta_Pred), !,
  objects:Parent_Meta_Pred,
  Parent_Meta_Pred =.. [Parent_Meta | Parent_Field_Names],
  is_set(Parent_Field_Names),

  atom_concat(Parent, '#t', Parent_Type),
  objects:current_predicate(Parent_Type, Parent_Type_Pred), !,
  objects:Parent_Type_Pred,
  Parent_Type_Pred =.. [Parent_Type | Parent_Field_Types],

  corteging(:, Parent_Field_Names, Parent_Field_Types, Parent_Fields_List),
                             
  append(Parent_Fields_List, Add_Fields, Class_Fields_List),
  maplist(field_name, Class_Fields_List, Class_Fields_Names),
  (is_set(Class_Fields_Names) -> true;
   write_log(['Field duplicates found when creating object', Class, 
               Class_Fields_Names]),
   fail
  ),
  assert_meta(Class, Parent, Class_Fields_List),
  get_key(Parent, Parent_Key),
  assert_key(Class, Parent_Key),
  assert_evals(Class, Parent),
  assert_copy(Class, Parent).
  
                             
%
% class_create(+Class, +Parent, +Add_Fields, +Key)
%
% Assert the new Class definition into the objects module,
% set a (compound) key to Key
%                             

class_create(Class, Parent, Fields, New_Key) :-

  class_create(Class, Parent, Fields),
  is_list(New_Key),
  get_key(Parent, Parent_Key),
  append(Parent_Key, New_Key, Key),
  assert_key(Class, Key).


field_name(Name : _, Name) :- !.

field_name(Name, Name).


field_type( _ : Type, Type) :- !.

field_type(_ , _).


assert_key(Class, Key) :-
  retractall(objects:key(Class, _)),
  Key = [] -> true
  ;
  is_list(Key),
  list_to_set(Key, Keys),
  assert(objects:key(Class, Keys)).
                             

assert_meta(Class, Parent, Fields) :-

  atom_concat(Class, '#', Class_Meta),
  atom_concat(Class_Meta, 't', Class_Types),

  % check already defined
  ( (  objects:current_predicate(Class_Meta, _) 
    ;  objects:current_predicate(Class_Types, _) 
    ;  (   objects:current_predicate(parent/2) 
       ->  (  objects:parent(Class, _) 
           ;  objects:parent(_, Class)
           )
       ;  false
       )
    ) 
  -> write_log(['Class', Class, 'is already defined.']), fail
  ;  true
  ),

  maplist(field_name, Fields, Field_Names),
  maplist(field_type, Fields, Field_Types),

  Class_Meta_Pred =.. [Class_Meta | Field_Names],
  Class_Type_Pred =.. [Class_Types | Field_Types],
  Parent_Pred =.. [parent, Class, Parent],
  objects:assert(Class_Meta_Pred),
  objects:assert(Class_Type_Pred),
  objects:assert(Parent_Pred).


%
% evals stays for *_v? predicates here
%
% If child declararion doesn't have parent's evals
% copy them from parent
%

assert_evals(Class, Parent) :-

  atom_concat(Parent, '?', Parent_Dc),
  atom_concat(Class, '?', Class_Dc),
  
  (objects:current_predicate(Parent_Dc, Parent_T),
   objects:clause(Parent_T, _),
   Parent_T =.. [Parent_Dc, _, Field, _],
   \+ (objects:current_predicate(Class_Dc, Class_T),
       objects:clause(Class_T, _),
       Class_T =.. [Class_Dc, _, Field, _]
       ),

   Class_T2 =.. [Class_Dc, A, Field, C],
   Parent_T2 =.. [Parent_Dc, A, Field, C],  
   assert(objects:(Class_T2 :- Parent_T2)),
   fail
   ; 
   true
   ).

% 
% Inherit copy from parent if not defined for Class
%

assert_copy(Class, Parent) :-

  (   objects:clause(copy(Class, _, _), _)
  ->  true
  ;   assert(objects:
             (copy(Class, From, To) :- copy(Parent, From, To))
      )
  ).


%
% class_parent(?Class, ?Parent)
%
                             
class_parent(Class, Parent) :-

  objects:current_predicate(parent, _) ->
  call(objects:parent, Class, Parent).
                             
%
% class_descendant(?Class, ?Descendant)
%
                             
class_descendant(Class, Descendant) :-

  class_ensure_created(Descendant),
  class_parent(Descendant, Class)
  ;
  class_parent(Descendant, X),
  class_descendant(Class, X).

% Get list of field names                             
                             
class_fields(Class, Fields) :-

  atom_concat(Class, '#', Descriptor),
  objects:current_functor(Descriptor, Arity),
  functor(Descr_Pred, Descriptor, Arity),
  objects:Descr_Pred,
  Descr_Pred =.. [Descriptor|Fields], !.
                             
%
% class_ensure_created(+Class)
%

class_ensure_created(Class) :-

  atom_concat(Class, '#', Meta),
  (
   objects:current_predicate(Meta, _) -> true

   ;

   atom(Class),

   Class = object_base_v -> true % object_base_v is implicitly
                                 % defined

   ;
                             
   (find_class_module(Class, Module_Path) -> true; %NB implies '!'
    write_log(['A definition module for the class', Class, 
               'is not found']), fail
   ),
   load_class_module(Class, Meta, Module_Path)
  ).


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
% get_key(?Class, ?Key)
%

get_key(Class, Key) :-

  atom(Class), !,
  class_ensure_created(Class),
  (objects:key(Class, Key) -> true ; Key = []).


%
% get_key(Object, ?Key)
%

get_key(Object, Key) :-

  compound(Object), !,
  functor(Object, Class, _),
  get_key(Class, Key).


get_key_value(Object, Key_Value) :-

  functor(Object, Class, _),
  get_key(Class, Key),
  named_args_unify(Object, Key, Key_Value), !.

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
  findall(Field, 'object_v?'(object_v, Field, _), Object_V_Fields),

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

%
% Low-level access
%

class_arg_num(Class, Arg_Num, Arg_Name) :-

   atom_concat(Class, '#', Descr_Functor),
   objects:current_predicate(Descr_Functor, Descr_Term),
   objects:Descr_Term,
   arg(Arg_Num, Descr_Term, Arg_Name), !.

% in the weak version it sets Arg_Num=0 when the field Arg_Name
% is absent
class_arg_num_weak(Class, Arg_Num, Arg_Name) :-

   atom_concat(Class, '#', Descr_Functor),
   objects:current_predicate(Descr_Functor, Descr_Term),
   objects:Descr_Term,
   (  arg(Arg_Num, Descr_Term, Arg_Name)
   -> true
   ;  Arg_Num = 0
   ), !.
                             
class_field_type(Class, Field, Type) :-

   class_arg_num(Class, Arg_Num, Field),
   atom_concat(Class, '#t', Type_Functor),
   objects:current_predicate(Type_Functor, Type_Term),
   objects:Type_Term,
   arg(Arg_Num, Type_Term, Type), !.
   