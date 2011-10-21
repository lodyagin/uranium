%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.

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

%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%  --------------------------------------------------------------

:- module(db_i,
          [
           db_clear_int/1,
           %db_erase/1,
           db_key_is_valid/1,
           db_recorded/2,
           db_recordz/2,
           db_unify_int/6,
           erase_conflicts/3,
           key_conflict/4,
           prolog:message/3
           ]).

:- use_module(library(ordsets)).
:- use_module(u(internal/objects_i)).
:- use_module(u(v)).

:- multifile prolog:message/3.
:- multifile db_recorded/3, db_erase/1, db_recordz/2.

db_clear_int(DB_Key) :-

   atom(DB_Key), !,
   (  recorded(DB_Key, _, Ref),
      erase(Ref),
      fail ; true
   ).

db_recorded(DB_Key, Object) :-

    atom(DB_Key), !,
    recorded(DB_Key, Object, Ref),
    u_object(Object),
    arg(1, Object, Class_Id),
    obj_field_int(Class_Id, db_ref, strict, Object,
                  recorded(Ref), _).

%db_recorded(DB_Key, Term, DB_Ref) :-

%    call_db_pred(DB_Key, recorded, [Term, DB_Ref]).
/*
db_erase(recorded(DB_Key, Ref)) :-

    !,
    (

db_erase(DB_Ref) :-

    call_db_pred(DB_Ref, erase, []).
*/

% db_recordz(+DB_Key, +Object)
%
% Set db_ref in Object

db_recordz(DB_Key, Object) :-

    atom(DB_Key), !,
    (  obj_field(Object, db_ref, Ref)
    -> var(Ref)
    ;  Ctx = context(db_recordz/2, _),
       throw(error(domain_error(db_object_v_desc, Object), Ctx))
    ),
%    record_control_pred(DB_Key, Object),
    recordz(DB_Key, Object, R),
    Ref = recorded(R).

% FIXME
%db_recordz(DB_Key, Term, Ref) :-


%record_control_pred(DB_Key, Object) :-

%   true. %TODO record '#keymaster'/1
   %functor(Object, Functor, _),


/*next_db_id(DB_Key, Id) :-

   (  recorded(DB_Key, '#next_db_id'(Id), Ref),
      erase(Ref)
   -> true
   ;  Id = 1 ),
   Next_Id is Id + 1,
   recordz(DB_Key, '#next_db_id'(Next_Id)).
*/

% db_erase_by_key(+DB_Key, +Key, +Key_Value)
%
% Erase all objects matching key
% Key_Value must be ground
%

/*
db_erase_by_key(DB_Key, Key, Key_Value) :-

   ground(Key_Value),
   ( db_unify_int(DB_Key, Key, strict, Key_Value,
                  Existing_Object),
     db_obj_field(Existing_Object, db_ref, Ref),
     db_erase(Ref),
     fail ; true ).
*/
%    call_db_pred(DB_Key, recordz, [Term, Ref]).

call_db_pred(DB_Key, Pred, Args) :-

    functor(DB_Key, DB, _),
    atom_concat('db_', DB, Module),
    use_module(library(Module)),
    format(atom(DB_Pred), '~a_~a', [DB, Pred]),
    apply(DB_Pred, [DB_Key|Args]).

db_key_is_valid(DB_Key) :-

   ground(DB_Key),
   (  atom(DB_Key)
   ;  compound(DB_Key),
      functor(DB_Key, pg, _)
   ), !.

% db_unify_int(+DB_Key, +Fields, +Weak, ?Values, -Object)
%
% In a case of Weak \== weak Fields must be ordset!

db_unify_int(DB_Key, Class_Id, Fields, Weak, Values, Object) :-

   (  Weak == weak
   -> true
   ;  % need to check presence of all Fields
      class_fields(Class_Id, _, _, Class_Fields),
      ord_subset(Fields, Class_Fields)
   ),

   % Construct new unbounded object for DB unification
   obj_construct_int(Class_Id, Fields, Weak, Values, Object),

   % bt on all matched records
   db_recorded(DB_Key, Object).


% erase_conflicts(+DB_Key, +Class_Id, @Object)
%
% Erase all key-conflicting objects from DB

erase_conflicts(DB_Key, Class_Id, Object) :-

  (  key_conflict(DB_Key, Class_Id, Object, Conflicting),
     obj_field(Conflicting, db_ref, DB_Ref),
     db_erase(DB_Ref),
     fail ; true ).

% key_conflict(+DB_Key, +Class_Id, @Object, -Conflicting)
%
% Return the conflicting object.
% Using of ground(Key_Value) ensures Object is not changed

key_conflict(DB_Key, Class_Id, Object, Conflicting) :-

   db_recorded(DB_Key, '#keymaster'(Key_Class_Id)),
   same_or_descendant(Key_Class_Id, false, Class_Id),
   % false means test all rebased classes as well

   get_key(Key_Class_Id, Key),
   obj_unify_int(Class_Id, Key, strict, Object, Key_Value),
   ground(Key_Value),           % unbounded key is not a key
   same_or_descendant(Key_Class_Id, false, DB_Class_Id),
   db_unify_int(DB_Key, DB_Class_Id, Key, strict, Key_Value,
                Conflicting).


prolog:message(db_key_exists(DB_Key, DB_Object, New_Object)) -->

   ['Duplicate DB key for DB ~a when put ~p'
   - [DB_Key, New_Object]],
   [ nl ],
   [ 'The existing object in DB: ~p' - [DB_Object]].

%prolog:message(db_ungrounded_key(Key, Key_Value, Action)) -->

%   ['When try ~a the ungrounded key ~p = ~p was found'
%   - [Action, Key, Key_Value]].

