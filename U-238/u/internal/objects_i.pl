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

:- module(objects_i,
          [
           class_all_fields/2,  % +Class_Id, -Fields
           class_arity/2,

           % @Pattern, +Class_Id, ?Native, ?Eval, ?Fields
           class_fields/5,

           class_id/2,
           class_new_fields/2,  % +Class_Id, -Fields
           class_primary_id/2,
           common_parent/3,
           fields_names_types/3,
           gen_class_id/2,
           gen_new_class_id/1,
           get_key/2,
           get_keymaster/2,
           is_rebased_class/1,
           list_inheritance/2,
           list_inheritance/3,
           obj_class_id/2,
           obj_construct_int/5,
           obj_field_int/7,
           obj_unify_int/6,
           parent/2,            % ?Id, ?Parent_Id
           same_or_descendant/3,%+Parent_Id, +No_Rebased, ?Desc_Id
           u_class/1,
           u_object/1,
           unbounded_fields/2, % +Obj, -Field_Names
           prolog:message/3
           ]).

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(u(ur_lists)).
:- use_module(u(internal/check_arg)).

:- multifile prolog:message/3.

:- dynamic objects:arity/2,    % Class_Id, Arity
           objects:class_id/3, % Class_Id, Id_Primary, Class
           objects:copy/4,     % Class_Id, Class_Name, From, To
           objects:downcast/4,

           objects:field/7,    % Class_Id, Field_Name, Obj, Value,
                               % Field_Type, Is_Native, Is_Eval

           objects:key/3,      % Class_Id, Keymaster_Id, Key (ordset)
           objects:module/2,
           objects:module_class_def/3, % Class, Parent, Module
           objects:parent/2, % Class_Id, Parent_Class_Id
           objects:pretty_print/4,
           objects:rebased_class/3,% Name, Parents, Id
           objects:typedef_flag/2.


class_all_fields(Class_Id, Fields) :-

   class_fields(_, Class_Id, _, false, Fields).


% class_arity(+Class_Id, -Arity)

class_arity(Class_Id, Arity) :-

   integer(Class_Id),
   (  objects:arity(Class_Id, Arity) -> true
   ;  throw(class_system_bad_state(
            'no objects:arity/2 for class id ~d' - Class_Id))
   ).


% class_fields(@Pattern, +Class_Id, ?Native, ?Eval, ?Fields)
% Get list of fields/types
% Native == true means no field from parent
% Native == false - only parent fields
% Native unbound - both
%
% Eval == true - only eval fields
% Eval == false - only non-eval fields
% Eval unbound - both
%
% It returns ordset (coz it uses setof)

class_fields(Pattern, Class_Id, Native, Eval, Fields) :-

   var(Pattern), !,

   class_fields(_-_, Class_Id, Native, Eval, Fields0),
   pairs_keys(Fields0, Fields).

class_fields(_-_, Class_Id, Native, Eval, Fields) :- !,

   class_fields2(Class_Id, Native, Eval, Fields).

class_fields(Pattern, Class_Id, Native, Eval, Fields) :-

   functor(Pattern, Symbol, 2),
   class_fields(_-_, Class_Id, Native, Eval, Fields0),
   pairs_replace_functor(Symbol, Fields0, Fields).

class_fields2(Class_Id, Native, Eval, Fields) :-

   (  setof(Field_Name-Field_Type,
        Obj^Value^Body^Native^Eval^ (objects:clause(
           field(Class_Id, Field_Name, Obj, Value, Field_Type,
                 Native, Eval),
           Body )),
        Fields0
      )
   -> predsort(compare_obj_fields, Fields0, Fields)
   ;  Fields = []
   ).

compare_obj_fields(Delta, E1-_, E2-_) :-

   compare(Delta, E1, E2).

class_id(Class_Id, Class) :-

   nonvar(Class_Id), !,
   objects:class_id(Class_Id, _, Class), !. % ensure no BT

% Nondet!
% Use it only for get all Class_Ids (primary + rebased)
% In the case you need a single id see class_primary_id
class_id(Class_Id, Class) :-

   objects:class_id(Class_Id, _, Class).


class_new_fields(Class_Id, Fields) :-

   class_fields(_, Class_Id, true, false, Fields).

% det
class_primary_id(Class, Class_Id) :-

   objects:class_id(Class_Id, true, Class), !.


% common_parent(+Class1_Id, +Class2_Id, -Cmn_Parent_Id)
%
% find the common parent's id

common_parent(Class1_Id, Class2_Id, Cmn_Parent_Id) :-

   list_inheritance(Class1_Id, List1),
   list_inheritance(Class2_Id, List2),
   common_head_rev(List1, List2, [Cmn_Parent_Id|_]).

%
% list_inheritance(+Class_Id, -List)
%
% represent inheritance as [class_id|...]
% e.g. list_inheritance(1, [0, 1]).

list_inheritance(Class_Id, List) :-

   list_inheritance(0, Class_Id, [], List).


list_inheritance(From_Id, To_Id, List) :-

   list_inheritance(From_Id, To_Id, [], List).


list_inheritance(Id, Id, List, [Id|List]) :- !.

list_inheritance(From_Id, To_Id, List0, List) :-

   objects:parent(To_Id, Parent_Id),
   list_inheritance(From_Id, Parent_Id, [To_Id|List0], List).


% fields_names_types(?Fields_Def, ?Names, ?Types)

fields_names_types([], [], []).

fields_names_types([Name:Type|FT], [Name|NT], [Type|TT]) :-

   nonvar(Type), !,
   (  var(Name) -> throw(error(instantiation_error, _))
   ; true ),
   (  \+ atom(Name) -> throw(error(type_error(atom, Name), _))
   ; true ),
   (  \+ atom(Type) -> throw(error(type_error(atom, Type), _))
   ; true ),
   fields_names_types(FT, NT, TT).

fields_names_types([Name|FT], [Name|NT], [_|TT]) :-

   (  var(Name) -> throw(error(instantiation_error, _))
   ; true ),
   (  \+ atom(Name) -> throw(error(type_error(atom, Name), _))
   ; true ),
   fields_names_types(FT, NT, TT).


gen_class_id(Class, Class_Id) :-

   nonvar(Class), var(Class_Id),
   (  class_id(Class_Id, Class)
   -> throw(class_exists(Class))
   ;  true ),

   gen_new_class_id(Class_Id).


gen_new_class_id(Class_Id) :-

   findall(Id, class_id(Id, _), Ids),
   max_list(Ids, Class_Id1),
   Class_Id is Class_Id1 + 1.


%
% get_key(+Class_Id, -Key)
%

get_key(Class_Id, Key) :-

  must_be(positive_integer, Class_Id),
  (objects:key(Class_Id, _, Key) -> true ; Key = []).


%
% get_keymaster(+Class_Id, -Keymaster_Id)
%

get_keymaster(Class_Id, Keymaster_Id) :-

  objects:key(Class_Id, Keymaster_Id, _).


is_rebased_class(Class_Id) :-

   nonvar(Class_Id),
   objects:class_id(Class_Id, false, _).


obj_class_id(Object, Class_Id) :-

   arg(1, Object, Class_Id).


parent(Id, Parent_Id) :-

   objects:parent(Id, Parent_Id), !,
   Id =\= 0.

obj_construct_int(Class_Id, Field_Names, Weak, Field_Values,
                  Object) :-

   Ctx = context(obj_construct_int/5, _),

   decode_arg([[throw, throws, strict, s],
               [unbound, weak, w],
               [fail, false, f]
              ], Weak, Weak1, Ctx),

   class_id(Class_Id, Class),
   class_arity(Class_Id, Arity),
   functor(Object, Class, Arity),
   arg(1, Object, Class_Id),
   obj_unify_int(Class_Id, Field_Names, Weak1, Object,
                 Field_Values, Ctx).

obj_field_int(Class_Id, Field_Name, Weak, Obj, Value, Type, Ctx)
:-

   % whether to be det or nondet
   (  nonvar(Field_Name) -> Det = t ; Det = f ),

   (
      (  Is_Eval = false,
         objects:field(Class_Id, Field_Name, Obj, Value, Type, _,
                     Is_Eval)
      ;
         Is_Eval = true,

         % each eval field must be evaluable only once
         class_fields(_, Class_Id, _, true, All_Eval_Fields),
         member(Field_Name, All_Eval_Fields),

         % BT on different eval preds (up to a hierarchy)
         find_eval_pred(Field_Name, Obj, Class_Id, Eval_Pred, Value)
      )

   *->

      ( Is_Eval = false
      -> (Det = t -> ! ; true)  %% return the fixed value
      ;
         call(Eval_Pred, Obj, Field_Name, Value),
         (Det = t -> ! ; true) %% return the evaluated value
      )
   ;
      % no such field or invalid value ?

      (  objects:clause(field(Class_Id, Field_Name, _, _, _, _,
                              _), _)
      -> fail % no value
      ;
         (  Weak = unbound
         -> (Det = t -> ! ; true) %% return an unbound value
         ;  Weak = fail
         -> fail
         ;  Weak = throw
         -> throw(error(no_object_field(Obj, Field_Name), Ctx))
         ;  Self_Ctx = context(obj_field_int/7, _),
            decode_arg([[unbound], [fail], [throw]], Weak, _,
                       Self_Ctx)
         )
      )
   ).


% Now the evaluation order can't be defined during class creation
% because of rebasing

% TODO move this discovery to class creation time
% Eval time must statically calls aready asserted function
% Thus, they should be asserted in the proper order

find_eval_pred(_, _, 0, _, _) :- fail. % no evals on object_base_v

find_eval_pred(Field, Object, Class_Id, Eval_Pred, Value) :-

  class_id(Class_Id, Class),
  atom_concat(Class, '?', Name),
  Eval_Term =.. [Name, Object, Field, Value],
  Body = Eval_Module:Eval_Term,
  (
     objects:clause(field(Class_Id, Field, Object, Value, _, _, true),
                    Body)
  *->
     % found it (and leave a choice point)
     Eval_Pred = Eval_Module:Name
  ;
     parent(Class_Id, Parent_Id),
     find_eval_pred(Field, Object, Parent_Id, Eval_Pred, Value)
  ).


obj_unify_int(_, [], _, _, [], _) :- !.

obj_unify_int(Class_Id, [Field|FT], Weak, Term, [Value|VT], Ctx)
:-

   obj_field_int(Class_Id, Field, Weak, Term, Value, _, Ctx),
   obj_unify_int(Class_Id, FT, Weak, Term, VT, Ctx).

% same_or_descendant(+Parent_Id, +No_Rebased, ?Desc_Id)

same_or_descendant(Id, No_Rebased, Id) :-

   (  No_Rebased == true
   -> \+ is_rebased_class(Id)
   ;  true ).

same_or_descendant(Parent_Id, No_Rebased, Desc_Id) :-

   Parent_Id \== Desc_Id,
   (  No_Rebased == true
   -> \+ is_rebased_class(Parent_Id)
   ;  true ),
   objects:parent(Id, Parent_Id),
   same_or_descendant(Id, No_Rebased, Desc_Id).


%% u_class(@Class)
%  True if Class is a valid name for an uranium class

u_class(Class) :-

   atom(Class),
   atom_concat(_, '_v', Class).


%% u_object(@Term)
%  True if Term is bound to an uranium object.

u_object(Term) :-

   compound(Term),
   functor(Term, Functor, _), %NB arity can be 0 (object_v)
   atom_concat(_, '_v', Functor).

% unbounded_fields(+Obj, -Field_Names)
%
% Return all names of unbound (var) noneval fields in Obj
% Attention: Field_Names is not ordered, just a simple list

unbounded_fields(Obj, Field_Names) :-

   obj_class_id(Obj, Class_Id),
   findall(Field,
           (objects:field(Class_Id, Field, Obj, Value, _, _,
                          false),
            var(Value)
           ),
           Field_Names
          ).

% Error messages for the Uranium object system

prolog:message(class_system_bad_state(Details)) -->

   ['The Uranium class system may be corrupted: '],
   [ nl ],
   [Details].

prolog:message(implementation_error(Format, Args)) -->

   ['Internal Uranium error: ', nl],
   [Format - Args].

prolog:message(class_exists(Class)) -->

   ['The class ~a is defined already' - [Class]].

prolog:message(class_inheritance_cycle(Graph)) -->

   ['There is a cycle in class inheritance: ~w'
    - Graph].

prolog:message(type_redefined(Type, Orig_Class)) -->

   ['Type ~a is already defined in class ~a'
    - [Type, Orig_Class]],
   [nl, 'The new definition was ignored.'].

prolog:message(invalid_object(Object, Details)) -->

   ['Invalid object passed: ~p ' - [Object]],
   [ '(', Details, ')' ].

prolog:message(no_object_field(Object, Field_Name)) -->

   ['There is no such field `~a'' in the object ~p'
    - [Field_Name, Object]].

prolog:message(undef_operation(Op_Name, Class_Id)) -->

   ['The operation `~a'' is not defined for class id ~d'
   - [Op_Name, Class_Id]].

prolog:message(bad_eval_result(Object, Field)) -->

   ['User-defined field `~a'' evaluation failed for ~p'
   - [Field, Object]].

prolog:message(bad_downcast_impl(Mode, Object, Class_From,
                                 Class_To, Result)) -->

   ['User-defined ~a implementation is bad:' - [Mode]],
   [ nl ],
   ['~a -> ~a transforms ~p to ~p'
   - [Class_From, Class_To, Object, Result]].

prolog:message(not_downcast(From_Class, To_Class)) -->

   ['~a -> ~a is not downcast' - [From_Class, To_Class]].


