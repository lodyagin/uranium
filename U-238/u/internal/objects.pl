%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012  Sergei Lodyagin
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

% Objects storage

:- module(objects,
          [
           arity/2,         % Class_Id, Arity
           class_id/3,      % Class_Id, Is_Primary, Class
           copy/4,          % Class_Id, Class_Name, From, To
           downcast/4,
           eval_field/5,    % Class, Field_Name, Obj, Value,
                            % Goal
           
           field/4,         % Class_Id, Field_Name, Obj, Value
           field_info/5,    % Class_Id, Field_Name, Field_Type,
                            % Is_Native, Is_Eval
           
           key/3,           % Class_Id, Keymaster_Id, Key (ordset)
           module/2,        % Class_Name, Module_Path
           
           module_class_def/3,  % Class, Parent, Main_Module_Class
           next_class_id/1,     % Class_Id
           parent_/2,           % Class_Id, Parent_Class_Id
           pretty_print/4,

           rebased_class/3,     % Name, Parents, Id
                                % it is used as a cache
                                % for both rebased and not
                                % rebased classes
           
           reinterpret/4,
           typedef_flag/2,

	   retract_object/2,    % ?ClassId, ?Class
           retractall_objects/0
          ]).

:- dynamic arity/2,
           class_id/3,
           copy/4,
           downcast/4,
           eval_field/5,
           field/4,
           field_info/5,
           key/3,         % Class_Id, Parent_Id, Key_Set
           module/2,
           module_class_def/3,
           next_class_id/1,
           parent_/2,
           pretty_print/4,
           rebased_class/3,
           reinterpret/4,
           typedef_flag/2.

retractall_objects :-
   retract_object(_, _),
   retractall(next_class_id(_)),
   retractall(pretty_print(_, _, _, _)),
   retractall(typedef_flag(_, _)),
   retractall(value_set(_, _, _, _, _)),
   retractall(value_options(_, _, _)).

retract_object(ClassId, Class) :-
   retractall(arity(ClassId, _)),
   retractall(class_id(ClassId, _, _)),
   retractall(class_id(_, _, Class)),
   retractall(copy(ClassId, _, _, _)),
   retractall(downcast(Class, _, _, _)),
   retractall(downcast(_, Class, _, _)),
   retractall(eval_field(Class, _, _, _, _)),
   retractall(field(ClassId, _, _, _)),
   retractall(field_info(ClassId, _, _, _, _)),
   retractall(key(ClassId, _, _)),
   retractall(module(Class, _)),
   retractall(module_class_def(Class, _, _)),
   retractall(parent_(ClassId, _)),
   retractall(rebased_class(_, _, ClassId)),
   retractall(reinterpret(Class, _, _, _)),
   retractall(reinterpret(_, Class, _, _)).



           
