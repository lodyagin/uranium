% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(db_auto_value_v,
          [
           db_auto_value/4,     % +DB_Key, +Class_Name,
                                % +Field_Name, ?New

           db_bind_auto/2,      % +DB_Key, ?Obj
           
           new_db_auto_value/5  % +DB_Key, +Class_Name,
                                % +Field_Name,:Pred, +Start_Value
           ]).

/** <module> Autogeneration of object field values when put objects into DB

  Usually it is not constructed by generic predicates but created
  in the db with new_db_auto_value/5.

  ---+++ Parent
  db_object_v.pl

  ---+++ New static fields
   $ class_name : the name of class for which the generator is
   installed
   $ field_name : the name of field for which the generator is
   installed
   $ auto_value : the last used value for (class_name, field_name)
   $ next_value_pred : a 2 args predicate which calculate the new
   value by the last used value, e.g., succ/2

  ---+++ Key
  =|[class_name, field_name]|=
*/

:- use_module(library(error)).
:- use_module(u(vd)).
:- use_module(u(internal/check_arg)).
:- use_module(u(util/lambda)).

:- meta_predicate db_auto_value(+, ?).
:- meta_predicate new_db_auto_value(+, +, 2, +).

new_class(db_auto_value_v, db_object_v,
          [class_name, field_name,
           next_value_pred,
           auto_value],
          [class_name, field_name]).


%% new_db_auto_value(+DB_Key, +Class_Name, +Field_Name, :Pred,
%%                   ?Start_Value) is det
%
%  Install the generator for the field Field_Name of the class
%  Class_Name. For any pair (Class_Name, Field_Name) only one
%  generator can be installed. Pred is a predicate accepting 2
%  args (+Old_Value, -New_Value). Start_Value is the Old_Value of
%  the first Pred call (not the first field value).

new_db_auto_value(DB_Key, Class_Name, Field_Name, Pred,
                  Start_Value) :-

   Ctx = context(new_db_auto_value/5, _),
   check_db_key(DB_Key, Ctx),
   check_inst(Class_Name, Ctx),
   check_class_arg(Class_Name, Ctx),
   check_field_name(Field_Name, Ctx),
   must_be(callable, Pred),

   db_construct(DB_Key, db_auto_value_v,
                [class_name, field_name,
                 next_value_pred,
                 auto_value],
                [Class_Name, Field_Name,
                 Pred,
                 Start_Value]).


%% db_auto_value(+DB_Key, ?Class_Name, ?Field_Name, ?New) is nondet
%
% Calculate New as a new auto value (with a help of
% =next_value_pred= field) and replace this singleton object in
% DB storing the last used value as =auto_value= for the next usage.
%
% Can fail if no db_auto_value_v stored in DB_Key for
% (Class_Name, Field_Name).
%
% If Class_Name or Field_Name is unbound return (Class_Name,
% Field_Name, New) for all matched db_auto_value_v objects from
% DB_Key (and update them with New value as well).

db_auto_value(DB_Key, Class_Name, Field_Name, New) :-

   Ctx = context(db_auto_value/2, _),
   check_db_key(DB_Key, Ctx),
   (  var(Class_Name) -> true
   ;  check_class_arg(Class_Name, Ctx)
   ),
   (  var(Field_Name) -> true
   ;  check_field_name(Field_Name, Ctx)
   ),
   db_auto_value_cmn(DB_Key, Class_Name, Field_Name, New, Ctx).

db_auto_value_cmn(DB_Key, Class_Name, Field_Name, New, _) :-
   
   named_args_unify(DB_Key, db_auto_value_v,
                    [class_name, field_name, auto_value,
                     next_value_pred],
                    [Class_Name, Field_Name, Auto_Value,
                     Pred],
                    Obj1),

   (  nonvar(New)
   -> true
   ;  call(Pred, Auto_Value, New)
   ),
   obj_rewrite(Obj1, [auto_value], _, [New], Obj),
   db_put_object(DB_Key, overwrite, Obj, _, replaced).


%% db_bind_auto(DB_Key, Obj) is det
%
% For every Obj field with existing db_auto_value_v object in
% DB_Key calculate a new auto value and unify with the Obj field
% if it is possible. In other case ignore the new value. But
% store the new value in db_auto_value_v in any case.
%
% If no db_auto_value_v objects are found in DB_Key for Obj it do
% nothing but always succeeds.
%
% When no db_auto_value_v object found for the class of Obj it
% try find db_auto_value_v for parents up to hierarchy.

db_bind_auto(DB_Key, Obj) :-

   Ctx = context(db_bound_auto/3, _),
   check_db_key(DB_Key, Ctx),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, _),

   foreach(
           (  db_auto_value_cmn(DB_Key, Class, Field, Value,
                                Ctx),
              obj_same_or_descendant(Obj, Class)
           ),
           ignore(obj_field(Obj, Field, Value))
          ).


   
