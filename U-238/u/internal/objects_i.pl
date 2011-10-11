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
           class_arity/2,
           class_fields/4,
           class_id/2,
           class_all_fields/2,
           class_new_fields/2,
           class_primary_id/2,
           common_parent/3,
           fields_names_types/3,
           gen_class_id/2,
           gen_new_class_id/1,
           get_key/2,
           is_rebased_class/1,
           list_inheritance/2,
           list_inheritance/3,
           obj_class_id/2,
           obj_construct_int/5,
           obj_field_int/6,
           obj_unify_int/5,
           parent/2,
           same_or_descendant/3,
           u_class/1,
           u_object/1,
           unbounded_fields/2, %+Obj, -Field_Names
           prolog:message/3
           ]).

:- use_module(library(lists)).
:- use_module(u(ur_lists)).

:- multifile prolog:message/3.

:- dynamic objects:arity/2,    % Class_Id, Arity
           objects:class_id/3, % Class_Id, Id_Primary, Class
           objects:copy/4,     % Class_Id, Class_Name, From, To
           objects:downcast/4,

           objects:field/7,    % Class_Id, Field_Name, Obj, Value,
                               % Field_Type, Is_Native, Is_Eval

           objects:fields/3,   % Class_Id, All_Fields, New_Fields
                               % (ordset)
           objects:key/2,      % Class_Id, Key (ordset)
           objects:module/2,
           objects:module_class_def/3, % Class, Parent, Module
           objects:parent/2, % Class_Id, Parent_Class_Id
           objects:pretty_print/4,
           objects:typedef_flag/2.

% class_arity(+Class_Id, -Arity)

class_arity(Class_Id, Arity) :-

   integer(Class_Id),
   (  objects:arity(Class_Id, Arity) -> true
   ;  throw(class_system_bad_state(
            'no objects:arity/2 for class id ~d' - Class_Id))
   ).

% class_fields(+Class_Id, ?Native, ?Eval, ?Fields)
% Get list of fields/types
% Native = true means no field from parent
% (Native = false - only parent fields)
%
% It returns ordset (coz it uses setof)

class_fields(Class_Id, Native, Eval, Fields) :-

   (  setof(Field_Name:Field_Type,
        Obj^Value^Body^ (objects:clause(
           field(Class_Id, Field_Name, Obj, Value, Field_Type,
                 Native, Eval),
           Body )),
        Fields
      )
   -> true
   ;  Fields = []
   ).

class_id(Class_Id, Class) :-

   nonvar(Class_Id), !,
   objects:class_id(Class_Id, _, Class), !. % ensure no BT

class_id(Class_Id, Class) :-

   objects:class_id(Class_Id, _, Class).


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


class_all_fields(Class_Id, Fields) :-

   objects:fields(Class_Id, Fields, _), !.


class_new_fields(Class_Id, New_Fields) :-

   objects:fields(Class_Id, _, New_Fields), !.


% parse Name : Type lists and check all values
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

  (objects:key(Class_Id, Key) -> true ; Key = []).


is_rebased_class(Class_Id) :-

   nonvar(Class_Id),
   objects:class_id(Class_Id, false, _).


obj_class_id(Object, Class_Id) :-

   arg(1, Object, Class_Id).


parent(Id, Parent_Id) :-

   objects:parent(Id, Parent_Id),
   Id =\= 0.

obj_construct_int(Class_Id, Field_Names, Weak, Field_Values,
                  Object) :-

   class_id(Class_Id, Class),
   class_arity(Class_Id, Arity),
   functor(Object, Class, Arity),
   arg(1, Object, Class_Id),
   obj_unify_int(Class_Id, Field_Names, Weak, Object,
                 Field_Values).

% NB evaluated fields can be also processed as `Weak'

obj_field_int(Class_Id, Field_Name, Weak, Obj, Value, Type) :-

   (  objects:field(Class_Id, Field_Name, Obj, Value, Type, _,
                    _)
   -> true
   ;  Weak == weak
   -> true
   ;  Weak == fail
   -> fail
      %%FIXME! no valid error (if eval fails)
   ;  fail %throw(no_object_field(Obj, Field_Name))
   ).


obj_unify_int(_, [], _, _, []) :- !.

obj_unify_int(Class_Id, [Field|FT], Weak, Term, [Value|VT]) :-

   obj_field_int(Class_Id, Field, Weak, Term, Value, _),
   obj_unify_int(Class_Id, FT, Weak, Term, VT).

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

prolog:message(implementation_error(Details)) -->

   ['Internal Uranium error: ', nl, Details].

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


