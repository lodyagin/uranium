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
           class_fields/3,
           class_id/2,
           class_all_fields/2,
           class_new_fields/2,
           class_primary_id/2,
           common_parent/3,
           fields_names_types/3,
           gen_class_id/2,
           gen_new_class_id/1,
           get_key/2,
           list_inheritance/2,
           list_inheritance/3,
           obj_class_id/2,
           u_class/1,
           u_object/1
           ]).

:- use_module(library(lists)).
:- use_module(library(ur_lists)).

:- multifile prolog:message/3.

:- dynamic objects:arity/2,    % Class_Id, Arity
           objects:class_id/3, % Class_Id, Id_Primary, Class
           objects:copy/4,     % Class_Id, Class_Name, From, To
           objects:downcast/4,
           
           objects:field/6,    % Class_Id, Field_Name, Obj, Value,
                               % Field_Type, Is_Native(Is_New)
           
           objects:fields/3,   % Class_Id, All_Fields, New_Fields
                               % (ordset)
           objects:key/2,      % Class_Id, Key (ordset)
           objects:module/2,
           objects:module_class_def/3, % Class, Parent, Module
           objects:parent/2, % Class_Id, Parent_Class_Id
           objects:pretty_print/4,
           objects:typedef_flag/2.

% class_fields(+Class_Id, ?Native, ?Fields)
% Get list of fields/types 
% Native = true means no field from parent
% (Native = false - only parent fields)
%
% It includes eval fields

class_fields(Class_Id, Native, Fields) :-

   (  setof(Field_Name:Field_Type,
        Obj^Value^Body^ (objects:clause(
           field(Class_Id, Field_Name, Obj, Value, Field_Type,
                 Native),
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


obj_class_id(Object, Class_Id) :-

   arg(1, Object, Class_Id).


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


prolog:message(class_system_bad_state(Details)) -->
                             
   ['The Uranium class system may be corrupted: '],
   [ nl ],
   [Details].

prolog:message(implementation_error(Details)) -->

   ['Internal Uranium error: ', nl, Details].

prolog:message(class_exists(Class)) -->
                             
   ['The class ~a is defined already' - [Class]].

prolog:message(invalid_object(Object, Details)) -->

   ['Invalid object passed: ~w ' - [Object]],
   [ '(', Details, ')' ].

prolog:message(no_object_field(Object, Field_Name)) -->

   ['There is no such field `~a'' in the object ~w'
    - [Field_Name, Object]].
                             
prolog:message(undef_operation(Op_Name, Class_Id)) -->

   ['The operation `~a'' is not defined for class id ~d'
   - [Op_Name, Class_Id]].

