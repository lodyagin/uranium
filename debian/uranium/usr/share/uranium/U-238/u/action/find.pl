% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
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

:- module(find, [find/6]).

:- use_module(u(ur_words)).
:- use_module(u(ur_lists)).
:- use_module(u(vd)).
:- use_module(u(v)).
:- use_module(u(tc_support/load_action_module)).
:- use_module(u(logging)).
:- use_module(u(internal/check_arg)).

%% find(+DB_Key, +SAT, +Object_Type, +Prop_List, +Actor0, -Actor) is det.
%
% Find all objects of the type _|Object_Type|__v in SAT.
% 
% The search is 2 stage process:
% 1. Call the ('find_all_' + Object_type)/3 pred from the module action/

find(DB_Key, SAT, Object_Type, Prop_List, Actor0, Actor) :-
  db_key_is_valid(DB_Key), !,
  check_db_key(DB_Key, context(find/6, _)),

  change_plural(Object_Type, Plural),
  decode_prop_list(Prop_List, Field_Names, Field_Values),
  atom_concat('find_all_', Plural, Find_All_Pred),
  Module_Name = Object_Type,
  load_action_module(SAT, Module_Name, Full_Module_Name),
  call(Full_Module_Name:Find_All_Pred, DB_Key, Actor0, Actor),
  filter_on_db(DB_Key, throw, Field_Names, Field_Values),

  write_log('find iteration 1:', [logger(dump_db), lf(1)]),
  dump_db(DB_Key), % logging

  atom_concat('fill_full_data_', Object_Type, Fill_Data_Pred),
  Fill_Data_Pred_Arity = 4,
  (
   current_predicate(Full_Module_Name:Fill_Data_Pred/Fill_Data_Pred_Arity) ->

   atom_concat(DB_Key, '.fill', DB_Key2),
   db_clear(DB_Key2),           % prepare place for iteration 2
   !,
   ( % get full data for all objects from DB1 into DB2
    db_recorded(DB_Key, Object1),
    call(Full_Module_Name:Fill_Data_Pred, DB_Key, DB_Key2, Actor, Object1),
    fail
   ;
    true
   ),

   write_log('find iteration 2:', [logger(dump_db), lf(1)]),
   dump_db(DB_Key2),

   db_merge(DB_Key, DB_Key2),

   write_log('find, after merging:', [logger(dump_db), lf(1)]),
   dump_db(DB_Key),
   dump_db(DB_Key2),

   filter_on_db(DB_Key, throw, Field_Names, Field_Values)

  ;
   % no fill_data predicate, no db2 in operation
   true
  ),

  write_log('find, after final filter:', [logger(dump_db), lf(1)]),
  dump_db(DB_Key).

%

%

find(SAT, Object_Type, Prop_List, Actor0, Actor. Found_Objects) :-

  change_plural(Object_Type, Plural),
  atom_concat('find.', Plural, DB_Key),
  db_clear(DB_Key),
  find(DB_Key, SAT, Object_Type, Prop_List, Actor0, Actor), !,
  db_to_list(DB_Key, _, Found_Objects).
