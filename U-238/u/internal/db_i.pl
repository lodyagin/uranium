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
	   db_conv_local_db/4,
           db_clear_int/1,
           db_erase_int/1,
           db_des/2,  % +DB_Key, ?Des
           db_key_is_valid/1,
           db_key_policy/3,  % +DB_Key, -Old, ?New
           db_object_class_int/2,
           db_recorded_int/2,
           db_recordz_int/2,
           erase_conflicts/3,
           key_conflict/4,
           named_args_unify_int/6,
           prolog:message/3
           ]).

:- use_module(library(ordsets)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/db_vocab)).
:- use_module(u(v)).

:- multifile prolog:message/3.
:- multifile db_recorded_int/2, db_erase_int/1, db_recordz_int/2.

db_des(DB_Key, Des) :-

   atom(DB_Key), !,
   Des = db_class_des(_, _, _, _, _, _),
   recorded(DB_Key, Des).

% db_key_policy(+DB_Key, -Old, ?New)
db_key_policy(DB_Key, Old, New) :-

   atom(DB_Key),
   var(New),  % do not set, only return
   !,

   Old = New,
   (  recorded(DB_Key, db_key_policy(Old)) -> true
   ;  Old = throw  % default is throw
   ).

db_key_policy(DB_Key, Old, New) :-

   atom(DB_Key), !,

   (  recorded(DB_Key, db_key_policy(Old), Ref)
   -> erase(Ref)
   ;  Old = throw  % default is throw
   ),

   recordz(DB_Key, db_key_policy(New)).

% DB maintain its own namespace of class ids

% DB service predicates:
%
% db_class_des(DB_Class_Id, DB_Parent_Class_Id,
%              Name, Arity, Fields, Key)
% db_next_class_id(Id).

db_erase_int(recorded(Ref)) :- !,

   erase(Ref).

db_next_class_id(DB_Key, Id) :-

   (   recorded(DB_Key, db_next_class_id(Id), Ref)
   ->  erase(Ref)
   ;   Id = 2 ),

   succ(Id, Next_Id),
   recordz(DB_Key, db_next_class_id(Next_Id)).

% Store class information in db and return new id
db_add_class(DB_Key, Local_Id, DB_Id, Des) :-

   parent(Local_Id, Local_Parent_Id),
   db_conv_local_db(DB_Key, Local_Parent_Id, DB_Parent_Id, _),

   db_next_class_id(DB_Key, DB_Id),
   class_arity(Local_Id, Arity), % arity = num fields+1
   class_all_fields(Local_Id, Fields),
   get_key(Local_Id, Key),
   class_id(Local_Id, Class),
   Des = db_class_des(DB_Id, DB_Parent_Id, Class, Arity,
		      Fields, Key),
   recordz(DB_Key, Des),

   % Parents are added before children.
   % Thus, need check keymaster only once - for parents
   (  get_keymaster(Local_Id, Local_Id)
   -> % the new key is introduced
      recordz(DB_Key, db_keymaster(DB_Id))
   ;
      true   % in hope the parent already added it
   ).

% class_db_local_id(+DB_Key, ?Local_Id, ?DB_Id, -Des)
% Convert local <-> db id for the class
% Des is unified db_class_des service term from db for this class
% Fail if no such class in db.

% in this case get answer from the cash
db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   db_vocab_local_db(DB_Key, Local_Class_Id, DB_Class_Id),
   !,
   (   DB_Class_Id =:= 1
   ->  true  % object_v is not stored in db
   ;   Des = db_class_des(DB_Class_Id, _, _, _, _, _),
       recorded(DB_Key, Des), !
   ).

%db_conv_local_db(+DB_Key, ?(+)Local_Class_Id, ?(-)DB_Class_Id,
%                 -Des)
db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   var(DB_Class_Id), integer(Local_Class_Id), !,

   class_id(Local_Class_Id, Class),
   (
       Class = object_v
   ->
       DB_Class_Id = 1  % DB_Class_Id for object_v is always 1
   ;
       (   recorded(DB_Key,
		    db_class_des(DB_Class_Id, A1, Class, A2,
				 DB_Fields, A3))
       ->  % a descriptor of this class is already in db

           (   db_local_db_compare_class(Local_Class_Id,
					 DB_Fields,
					 Local_Fields)
	   ->  true
	   ;   class_all_fields(Local_Class_Id, Local_Fields),
	       throw(error(class_fields_mismatch(
	          DB_Key, Class, Local_Fields, DB_Fields)))
	   ),

           Des = db_class_des(DB_Class_Id, A1, Class, A2,
			      DB_Fields, A3)
       ;
           db_add_class(DB_Key, Local_Class_Id, DB_Class_Id, Des)
       )
   ),
   db_vocab_local_db_add(DB_Key, Local_Class_Id, DB_Class_Id).

db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   var(Local_Class_Id), integer(DB_Class_Id), !,

   (   DB_Class_Id =:= 1   % object_v
   ->  class_primary_id(object_v, Local_Class_Id)
   ;   Des = db_class_des(DB_Class_Id, DB_Parent_Id, Class, _,
		       DB_Fields, _),
       !,

       (   recorded(DB_Key, Des) -> true
       ;   throw(error(invalid_db_class_id(DB_Key,
					   DB_Class_Id)))
       ),

       class_id(Local_Class_Id, Class), % try next class id

       % check class compatibility (can BT to next local id)
       db_local_db_compare_class(Local_Class_Id, DB_Fields,
				 _),

       % Check the parent
       db_conv_local_db(DB_Key, _, DB_Parent_Id, _),
       ! % no more BT
   ),
   db_vocab_local_db_add(DB_Key, Local_Class_Id, DB_Class_Id).


%db_local_db_compare_class(+Local_Class_Id, +DB_Fields,
%		          -Local_Fields)
% Compare local and db class
% Fail if mismatch
db_local_db_compare_class(Local_Class_Id, DB_Fields,
			  Local_Fields)
:-

   class_all_fields(Local_Class_Id, Local_Fields),
   Local_Fields == DB_Fields.


db_clear_int(DB_Key) :-

   atom(DB_Key), !,
   (  recorded(DB_Key, _, Ref),
      erase(Ref),
      fail ; true
   ),
   db_vocab_clear(DB_Key).

%db_object_class_int(+DB_Key, -Local_Class_Id)
% unify Local_Class_Id with all classes in DB on BT
db_object_class_int(DB_Key, Local_Class_Id) :-

   atom(DB_Key), !,
   recorded(DB_Key, db_class_des(DB_Class_Id, _, _, _, _, _)),
   db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, _).

% db_recorded_int(+DB_Key, ?L_Object)
db_recorded_int(DB_Key, L_Object) :-

    atom(DB_Key), nonvar(L_Object), !,
    Ctx = context(db_recorded_int/2, _),

    % check whether L_Object has db_ref
    arg(1, L_Object, Local_Class_Id),
    obj_field_int(Local_Class_Id, db_ref, throw, L_Object, _, _,
                  Ctx),

    object_local_db(DB_Key, L_Object, DB_Object),

    % find the matched record
    recorded(DB_Key, DB_Object, Ref),

    object_local_db(DB_Key, L_Object1, DB_Object),

    % populate values from db
    L_Object = L_Object1,

    % store the db record reference
    obj_field_int(Local_Class_Id, db_ref, throw, L_Object,
                  recorded(Ref), _, Ctx).

% the case of free L_Object, unify with all records in DB
db_recorded_int(DB_Key, L_Object) :-

    atom(DB_Key), var(L_Object), !,
    Ctx = context(db_recorded_int/2, _),

    % BT on all classes
    recorded(DB_Key, db_class_des(_, _, Class, Arity, _, _)),

    % BT on all class records
    functor(DB_Object, Class, Arity),
    recorded(DB_Key, DB_Object, Ref),

    object_local_db(DB_Key, L_Object, DB_Object),

    % inplant the db reference
    arg(1, L_Object, Local_Class_Id),
    obj_field_int(Local_Class_Id, db_ref, throw, L_Object,
                  recorded(Ref), _, Ctx).


%db_recorded(DB_Key, Term, DB_Ref) :-

%    call_db_pred(DB_Key, recorded, [Term, DB_Ref]).
/*
%db_erase_int(DB_Ref) :-

    call_db_pred(DB_Ref, erase, []).
*/

% db_recordz_int(+DB_Key, +Object)
%
% Set db_ref in Object

db_recordz_int(DB_Key, Object0) :-

    atom(DB_Key), !, % this is prolog DB version
    Ctx = context(db_recordz_int/2, _),
    obj_field(Object0, db_ref, Ref),
    (  var(Ref) -> true
    ;  throw(error(domain_error(unbound_db_ref, Object0),Ctx))
    ),

    object_local_db(DB_Key, Object0, Object),

    recordz(DB_Key, Object, R),
    Ref = recorded(R).


%object_local_db(+DB_Key, ?Local_Object, ?DB_Object)
% Convert between local and db object term
object_local_db(DB_Key, Local_Object, DB_Object) :-

    % it is from Local to DB case
    nonvar(Local_Object), var(DB_Object), !,
    Ctx = context(object_local_db/3, _),

    % get db id (and store service predicates if necessary)
    arg(1, Local_Object, Local_Class_Id),
    db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id,
		     Des),
    Des = db_class_des(_, _, Class, _, Fields, _),

    % replace the id
    obj_unify_int(Local_Class_Id, Fields, throw,
		  Local_Object, DB_Field_Vals, Ctx),

    DB_Object =.. [Class, DB_Class_Id | DB_Field_Vals].


object_local_db(DB_Key, Local_Object, DB_Object) :-

    % it is from DB to Local case
    var(Local_Object), nonvar(DB_Object), !,
    Ctx = context(object_local_db/3, _),

    % get local class id
    arg(1, DB_Object, DB_Class_Id),
    db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id,
		     Des),
    class_id(Local_Class_Id, Class),%get the local class name

    Des = db_class_des(_, _, _, _, Fields, _),

    % replace the id
    % <NB> DB_Object should be field-compatible with Local_Object
    obj_unify_int(Local_Class_Id, Fields, throw,
		  DB_Object, DB_Field_Vals, Ctx),

    Local_Object =.. [Class, Local_Class_Id| DB_Field_Vals].


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
     db_erase_int(Ref),
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

%db_unify_int(DB_Key, Class_Id, Fields, Weak, Values, Object) :-
%
%   (  Weak == weak
%   -> true
%   ;  % need to check presence of all Fields
%      class_fields(Class_Id, _, _, Class_Fields),
%      ord_subset(Fields, Class_Fields)
%   ),
%
%   % Construct new unbounded object for DB unification
%   obj_construct_int(Class_Id, Fields, Weak, Values, Object),
%
%   % bt on all matched records
%   db_recorded(DB_Key, Object).


% erase_conflicts(+DB_Key, +Class_Id, @Object)
%
% Erase all key-conflicting objects from DB

erase_conflicts(DB_Key, Class_Id, Object) :-

  (  key_conflict(DB_Key, Class_Id, Object, Conflicting),
     obj_field(Conflicting, db_ref, DB_Ref),
     db_erase_int(DB_Ref),
     fail ; true ).

% key_conflict(+DB_Key, +Class_Id, @Object, -Conflicting)
% nondet 
%
% Return the conflicting objects.
% Using of ground(Key_Value) ensures Object is not changed

key_conflict(DB_Key, Class_Id, Object, Conflicting) :-

   Ctx = context(key_conflict/4, _),
   recorded(DB_Key, db_keymaster(DB_Key_Class_Id)),
   db_conv_local_db(DB_Key, Key_Class_Id, DB_Key_Class_Id, Des),
   same_or_descendant(Key_Class_Id, false, Class_Id),
   % false means test all rebased classes as well

   Des = db_class_des(_, _, _, _, _, Key),
   obj_unify_int(Class_Id, Key, throw, Object, Key_Value, Ctx),
   %ground(Key_Value),           % unbounded key is not a key
   named_args_unify_int(DB_Key, throw, Des, Key, Key_Value,
                        Conflicting).
   


named_args_unify_int(DB_Key, Option, Des, Field_Names, Values,
                     Term) :-

   Des = db_class_des(DB_Class_Id, _, _, _, _, _),
   db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, _),
   % now the class is definitly loaded

   obj_construct_int(Local_Class_Id, Field_Names, Option, Values,
                     Term0),
   obj_rebase((object_v -> db_object_v), Term0, Term),
   db_recorded_int(DB_Key, Term).


prolog:message(db_system_bad_state(Format, Args)) -->

   ['The Uranium DB system may be corrupted: '],
   [ nl ],
   [Format - Args].

prolog:message(bad_db(DB_Key, Format, Args)) -->

   ['Bad DB ~w~n' - [DB_Key]],
   [Format - Args].

prolog:message(db_key_exists(DB_Key, DB_Object, New_Object)) -->

   ['Duplicate DB key for DB ~a when put ~p'
   - [DB_Key, New_Object]],
   [ nl ],
   [ 'The existing object in DB: ~p' - [DB_Object]].

prolog:message(class_fields_mismatch(DB_Key, Class, Local, DB))
-->

   ['The DB ~a has another fields for the class ~a~n'
    - [DB_Key, Class]],
   ['our fields: ~w ~ndb fields: ~w' - [Local, DB]].

prolog:message(invalid_db_class_id(DB_Key, DB_Class_Id)) -->

   ['The db class id ~w is not found in db ~w'
    - [DB_Class_Id, DB_Key]].

%prolog:message(db_ungrounded_key(Key, Key_Value, Action)) -->

%   ['When try ~a the ungrounded key ~p = ~p was found'
%   - [Action, Key, Key_Value]].











