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
           db_bind_auto/2      % +DB_Key, ?Obj
           ]).

/** <module> Autogeneration of object field values when put objects into DB

  ---+++ Parent
  db_object_v.pl

  ---+++ New static fields
   * class_name
   the name of class for which the generator is designated
  
   * field_name
   the name of field for which the generator is designated
  
   * auto_value_seed
   the last used value (= seed)
  
   * next_seed_pred
   a 2 args predicate which calculate the new value by the last
   used value, e.g., succ/2

  ---+++ New evaluated fields

   * auto_value

   It is calculated as =|next_value_pred(auto_value_seed,
   auto_value)|=.

   If the object has (db_key, db_ref) replace the object in DB with
   the new object storing =auto_value= as =auto_value_seed=.
  
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
           next_seed_pred,
           auto_value_seed],
          [class_name, field_name]).


'db_auto_value_v?'(Obj, auto_value, New_Value) :-

   obj_field(Obj, next_seed_pred, Pred),
   (  var(Pred) -> true
   ;
      obj_field(Obj, auto_value_seed, Old_Value),
      call(Pred, Old_Value, New_Value),

      (  named_args_weak_unify(Obj, [db_key, db_ref], DB_Addr),
         ground(DB_Addr)
      ->
         DB_Addr = [DB_Key, _],
         obj_rewrite(Obj, [auto_value_seed], _, [New_Value], Obj1),
         db_put_object(DB_Key, overwrite, Obj1, _, replaced)
      ;
         true
      )
   ).

%% db_bind_auto(DB_Key, Obj) is det
%
% For every Obj field with an existing db_auto_value_v (or its
% descendant) object in DB_Key calculate a new auto value and
% unify with the Obj field if it is possible. In other case
% ignore the new value. But store the new value in
% db_auto_value_v in any case.
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

   findall(p(Field, Value),
           (  % up to hierarchy
              obj_same_or_descendant(Obj, Class_Name),
              named_args_unify(DB_Key, _,
                               [class_name, field_name, auto_value],
                               [Class_Name, Field, Value],
                               AV_Obj),
              obj_same_or_descendant(AV_Obj, db_auto_value_v)
           ),
           Setters),

   foreach(member(p(Field, Value), Setters),
           ignore(obj_field(Obj, Field, Value))
          ).


   
