%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.

%  Copyright (C) 2012, Kogorta OOO Ltd
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
           db_erase_int/2,   % +DB_Key, +Object
           db_des/2,         % +DB_Key, ?Des
           db_functor_des/4, % +DB_Key, ?Functor, -Des, +Ctx
           db_key_is_valid/1,
           db_key_policy/3,  % +DB_Key, -Old, ?New
           db_set_callbacks/2,
           db_name_int/1,    % ?DB_Key
           db_object_class_int/2,
           db_recorded_int/2,
           db_record_int/4,  % +DB_Key, +Order, +Object, +Ctx
           erase_conflicts/3,
           key_conflict/4,
           named_args_unify_int/6,
           prolog:message//1
           ]).

:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/class_create)).
:- use_module(u(internal/db_vocab)).
:- use_module(u(v)).
:- use_module(u(internal/ur_debug)).

/** <module> Internal DB preds for use only from Uranium itself.

  It is unified interface which hides different specific DB
  implementations.
*/

:- multifile db_recorded_int/2, db_erase_int/2, db_record_int/4.

:- dynamic db_class_des/8,     % DB_Key, DB_Class_Id, DB_Parent_Class_Id,
                               % Name, Arity, Fields, Key, Parents (names)
           db_key_policy/2,
           db_after_put_callback/2,  % DB_Key, Pred
           db_next_class_id_/2,
           db_keymaster/2.     % DB_Key, Class_Name

db_des(DB_Key, Des) :-

   atom(DB_Key), !,
   Des = db_class_des(A, B, C, D, E, F, G),
   db_class_des(DB_Key, A, B, C, D, E, F, G).

% db_functor_des(+DB_Key, ?Functor, -Des, +Ctx)
% nondet
%
% Unify Des with all classes in DB matched with Functor

db_functor_des(DB_Key, Functor, Des, Ctx) :-

   % TODO UT on different rebased versions with the same functor
   % <NB> the class Functor can not be defined locally
   Des = db_class_des(_, _, Functor, _, _, _, _),
   db_des(DB_Key, Des),

   % check the Functor as a correct class name
   (  nonvar(Functor), u_class(Functor) -> true
   ;  throw(error(bad_db(DB_Key, 'Bad class name: ~w', [Functor]),
                  Ctx))
   ).


%% db_key_policy(+DB_Key, -Old, ?New) is det
db_key_policy(DB_Key, Old, New) :-

   atom(DB_Key),
   var(New),  % do not set, only return
   !,

   Old = New,
   (  db_key_policy(DB_Key, Old) -> true
   ;  Old = throw  % default is throw
   ).

db_key_policy(DB_Key, Old, New) :-

   atom(DB_Key), !,

   (  retract(db_key_policy(DB_Key, Old))
   -> true
   ;  Old = throw  % default is throw
   ),

   assertz(db_key_policy(DB_Key, New)).

%% db_set_callbacks(+DB_Key, :After_Put_Callback) is det.

:- meta_predicate db_set_callbacks(+, 1).

db_set_callbacks(DB_Key, After_Put_Callback) :-
   must_be(ground, DB_Key),
   must_be(callable, After_Put_Callback),
   retractall(db_after_put_callback(DB_Key, _)),
   assertz(db_after_put_callback(DB_Key, After_Put_Callback)).

% db_name_int(?DB_Key, +Ctx)

db_name_int(DB_Key) :-

   once((var(DB_Key) ; atom(DB_Key))),
   db_next_class_id_(DB_Key, _).


% DB maintain its own namespace of class ids

% DB service predicates:
%
% db_class_des(DB_Class_Id, DB_Parent_Class_Id,
%              Name, Arity, Fields, Key, Parents)
% db_next_class_id(Id).

db_erase_int(DB_Key, Object) :-
   atom(DB_Key), !,
   Ctx = context(db_erase_int/2, _),
   arg(1, Object, Local_Id),
   obj_field(Object, db_class, DB_Class_Id),
   class_all_fields(Local_Id, All_Fields),
   ord_del_element(All_Fields, db_ref, Reset_Fields),
   obj_reset_fields_int(Local_Id, Reset_Fields, Object, Object1,
                        throw, Ctx),
   object_local_db(DB_Key, Local_Id-Object1, DB_Class_Id-DB_Object),
   retract(DB_Object), !.

db_next_class_id(DB_Key, Id) :-

   atom(DB_Key), !,
   (   retract(db_next_class_id_(DB_Key, Id))
   ->  true
   ;   Id = 2 ),

   succ(Id, Next_Id),
   assertz(db_next_class_id_(DB_Key, Next_Id)).

% Store class information in db and return new id
db_add_class(DB_Key, Local_Id, DB_Id, Des) :-

   nonvar(Local_Id), var(DB_Id), !,
   parent(Local_Id, Local_Parent_Id),
   db_conv_local_db(DB_Key, Local_Parent_Id, DB_Parent_Id, _),

   db_next_class_id(DB_Key, DB_Id),
   class_arity(Local_Id, Arity), % arity = num fields+1
   class_all_fields(Local_Id, Fields),
   get_key(Local_Id, Key),
   class_id(Local_Id, Class),
   class_parents_as_names(Local_Id, Parents),
   assertz_pred(vd,
                db_class_des(DB_Key, DB_Id, DB_Parent_Id, Class,
                             Arity, Fields, Key, Parents)
               ),
   Des = db_class_des(DB_Id, DB_Parent_Id, Class, Arity, Fields, Key,
                      Parents),
   dynamic(Class/Arity),

   % Parents are added before children.
   % Thus, need check keymaster only once - for parents
   (  get_keymaster(Local_Id, Local_Id)
   -> % the new key is introduced
      class_id(Local_Id, Class_Name),
      assertz(db_keymaster(DB_Key, Class_Name))
   ;
      true   % in hope the parent already added it
   ).

% Retrieve class information from db and create new local class
db_add_class(DB_Key, Local_Id, DB_Id, Des) :-

   nonvar(DB_Id), var(Local_Id), !,
   Ctx = context(db_add_class/4, _),
   Des = db_class_des(_, DB_Parent_Id, Class, _, Fields, Key, Parents1),
   db_conv_local_db(DB_Key, Local_Parent_Id, DB_Parent_Id, _),
   (  class_primary_id(Class, _)
   -> % new rebased
      Parents2 = [object_base_v|Parents1],
      reverse(Parents2, Parents),
      class_rebase_int([Class|Parents], [Local_Id|_], _, Ctx)
   ;  % new class
      class_id(Local_Parent_Id, Parent),
      class_create_cmn(Class, Parent, Fields, Key, Local_Id, Ctx)
   ).

class_parents_as_names(Local_Class_Id, Parents) :-

   list_inheritance_names(Local_Class_Id, [_|Parents0]),
                                % skip object_base_v

   append(Parents, [_], Parents0), !.

% class_db_local_id(+DB_Key, ?Local_Id, ?DB_Id, -Des)
% Convert local <-> db id for the class
% Des is unified db_class_des service term from db for this class
% Fail if no such class in db.

% in this case get answer from the cash
db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   db_vocab_local_db(DB_Key, Local_Class_Id, DB_Class_Id),
   !,
   (   DB_Class_Id =:= 1
   ->  % object_v is not stored in db
       Des = db_class_des(1, 0, object_v, 0, [], [], [])
   ;
       db_class_des(DB_Key, DB_Class_Id, A, B, C, D, E, F), !,
       Des = db_class_des(DB_Class_Id, A, B, C, D, E, F)
   ).

%db_conv_local_db(+DB_Key, ?(+)Local_Class_Id, ?(-)DB_Class_Id,
%                 -Des)
% local -> db
db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   var(DB_Class_Id), integer(Local_Class_Id), !,

   (  class_path(db_object_v-_, Class-Local_Class_Id, _, _)
   -> true
   ;  class_id(Local_Class_Id, Class),
      (  Class == object_v -> true
      ;  throw(error(must_be_descendant_of(Class, db_object_v), _))
      )
   ),
   (
       Class = object_v
   ->
       DB_Class_Id = 1,  % DB_Class_Id for object_v is always 1
       Des = db_class_des(1, 0, object_v, 0, [], [], [])
   ;
       (   db_class_des(DB_Key, DB_Class_Id, A1, Class, A2,
                        DB_Fields, A3, Parents)
       ->  % a descriptor of this class is already in db

           class_parents_as_names(Local_Class_Id, Local_Parents),
           (   Local_Parents == Parents
           ->  true
           ;
               throw(error(class_parents_mismatch(DB_Key, Class,
                                   Local_Parents, Parents), _))
           ),
           class_all_fields(Local_Class_Id, Local_Fields),
           (   Local_Fields == DB_Fields
	   ->  true
	   ;
	       throw(error(class_fields_mismatch(
	          DB_Key, Class, Local_Fields, DB_Fields), _))
	   ),

           Des = db_class_des(DB_Class_Id, A1, Class, A2,
			      DB_Fields, A3, Parents)
       ;
           db_add_class(DB_Key, Local_Class_Id, DB_Class_Id, Des)
       )
   ),
   db_vocab_local_db_add(DB_Key, Local_Class_Id, DB_Class_Id).

% db -> local
db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, Des) :-

   var(Local_Class_Id), integer(DB_Class_Id), !,

   (   DB_Class_Id =:= 1,   % object_v
       Des = db_class_des(1, 0, object_v, 0, [], [], [])
   ->  class_primary_id(object_v, Local_Class_Id)
   ;   Des = db_class_des(DB_Class_Id, DB_Parent_Id, Class, DBF1,
		       DB_Fields, DBF2, DB_Parents),
       !,

       (   db_class_des(DB_Key, DB_Class_Id, DB_Parent_Id, Class,
                        DBF1, DB_Fields, DBF2, DB_Parents)
       -> true
       ;   throw(error(invalid_db_class_id(DB_Key,
					   DB_Class_Id)))
       ),

       (   class_id(Local_Class_Id, Class), % try next class id
           % check class compatibility (can BT to next local id)
           class_parents_as_names(Local_Class_Id, DB_Parents)
       ->  true
       ;   % add the new local class
           db_add_class(DB_Key, Local_Class_Id, DB_Class_Id, Des)
       ),
       class_all_fields(Local_Class_Id, Local_Fields),
       (   Local_Fields == DB_Fields
       ->  true
       ;
           print_message(warning,
                         class_fields_mismatch(DB_Key, Class,
                            Local_Fields, DB_Fields)),
           fail
       ),

       %FIXME on fail prev pred make the appropriate rebasing here

       % Check the parent
       db_conv_local_db(DB_Key, _, DB_Parent_Id, _),
       ! % no more BT
   ),
   db_vocab_local_db_add(DB_Key, Local_Class_Id, DB_Class_Id).


db_clear_int(DB_Key) :-

   atom(DB_Key), !,
   (  db_des(DB_Key, db_class_des(DB_Id, _, _, _, _, _, _)),
      db_conv_local_db(DB_Key, Local_Id, DB_Id, _),
      obj_construct_int(Local_Id, [db_key], throw, [DB_Key], Obj0),
      object_local_db(DB_Key, Local_Id-Obj0, DB_Id-Obj),
      retractall(Obj),
      fail ; true
   ),
   retractall(db_key_policy(DB_Key, _)),
   retractall(db_after_put_callback(DB_Key, _)),
   retractall(db_next_class_id_(DB_Key, _)),
   retractall(db_keymaster(DB_Key, _)),
   retractall(db_class_des(DB_Key, _, _, _, _, _, _, _)),
   debug(vd, '~p',
         retractall(db_class_des(DB_Key, _, _, _, _, _, _, _))),
   db_vocab_clear(DB_Key),

   assertz(db_keymaster(DB_Key, object_base_v)),
   debug(vd, '~p',
         assertz(db_keymaster(DB_Key, object_base_v))).

%db_object_class_int(+DB_Key, -Local_Class_Id)
% unify Local_Class_Id with all classes in DB on BT
db_object_class_int(DB_Key, Local_Class_Id) :-

   atom(DB_Key), !,
   db_class_des(DB_Key, DB_Class_Id, _, _, _, _, _, _),
   db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, _).

% db_recorded_int(+DB_Key, ?L_Object)
db_recorded_int(DB_Key, L_Object) :-

    atom(DB_Key), nonvar(L_Object), !,
    Ctx = context(db_recorded_int/2, _),

    % check whether L_Object is a db_object_v descendant
    arg(1, L_Object, Local_Class_Id),
    (  same_or_descendant(Local_Class_Id, _, db_object_v)
    -> true
    ;  throw(error(domain_error(db_object_v_descendant, L_Object),
                   Ctx))
    ),

    obj_rewrite_int(Local_Class_Id, L_Object, throw,
                    [db_key], _, [DB_Key], L_Object1, Ctx),

    object_local_db(DB_Key, Local_Class_Id-L_Object1, DB_Class_Id-DB_Object),

    % find the matched record
    call(DB_Object),

    object_local_db(DB_Key, Local_Class_Id-L_Object2, DB_Class_Id-DB_Object),

    % populate values from db
    L_Object = L_Object2.

    % store the db key and record references
    % obj_field_int(Local_Class_Id, db_key, throw, L_Object,
    %               DB_Key, _, Ctx),
    % obj_field_int(Local_Class_Id, db_ref, throw, L_Object,
    %               Ref, _, Ctx).

% the case of free L_Object, unify with all records in DB
db_recorded_int(DB_Key, L_Object) :-

    atom(DB_Key), var(L_Object), !,
    Ctx = context(db_recorded_int/2, _),

    % BT on all classes
    db_class_des(DB_Key, DB_Class_Id, _, Class, Arity, _, _, _),

    % BT on all class records
    functor(DB_Object0, Class, Arity),
    arg(1, DB_Object0, DB_Class_Id),
    object_local_db(DB_Key, Local_Id-L_Object0, DB_Class_Id-DB_Object0),
    obj_unify_int(Local_Id, [db_key], throw, L_Object0, [DB_Key],
                  Ctx),
    object_local_db(DB_Key, Local_Id-L_Object0, DB_Class_Id-DB_Object),
    call(DB_Object),

    object_local_db(DB_Key, Local_Id-L_Object, DB_Class_Id-DB_Object).

    % inplant the db key and reference
    %arg(1, L_Object, Local_Class_Id),
    %obj_field_int(Local_Class_Id, db_key, throw, L_Object,
    %              DB_Key, _, Ctx),
    %obj_field_int(Local_Class_Id, db_ref, throw, L_Object,
    %              Ref, _, Ctx).


%db_recorded(DB_Key, Term, DB_Ref) :-

%    call_db_pred(DB_Key, recorded, [Term, DB_Ref]).
/*
%db_erase_int(DB_Ref) :-

    call_db_pred(DB_Ref, erase, []).
*/

%% db_record_int(+DB_Key, +Order, +Object, +Ctx) is det.
%
% Set db_ref (and db_key if it is unbound) in Object
% Order = recordz | recorda

db_record_int(DB_Key, Order, Object0, Ctx) :-

    atom(DB_Key), !, % this is prolog DB version
    arg(1, Object0, Class_Id),
    obj_unify_int(Class_Id,
                  [db_key, db_ref, db_class], throw, Object0,
                  [DB_Key_Val, DB_Ref, DB_Class_Id], Ctx),

    (   DB_Key_Val = DB_Key -> true
    ;   throw(error(domain_error(unbound_or_same_db_key,
                                 DB_Key_Val), Ctx))
    ),
    (   var(DB_Ref) -> true
    ;   throw(error(domain_error(unbound_db_ref, DB_Ref),Ctx))
    ),
    (   var(DB_Class_Id) -> true
    ;   throw(error(domain_error(unbound_db_class, DB_Class_Id),Ctx))
    ),

    object_local_db(DB_Key, Class_Id-Object0, DB_Class_Id-Object),

    next_db_ref(DB_Ref),
    (   Order = recordz
    ->  assertz(Object)
    ;   asserta(Object)
    ),

    (  db_after_put_callback(DB_Key, After_Put_CB)
    -> call(After_Put_CB, Object0)
    ;  true
    ).


next_db_ref(Ref) :-

   flag(db_ref, Old_Ref, Old_Ref),
   succ(Old_Ref, Ref),
   flag(db_ref, _, Ref).

%object_local_db(+DB_Key, ?Local_Class_Id-?Local_Object, ?DB_Class_Id-?DB_Object)
% Convert between local and db object term
object_local_db(DB_Key, Local_Class_Id-Local_Object, DB_Class_Id-DB_Object) :-
    % TODO: db_key, db_ref and db_class can not be included
    % in a db object

    % it is from Local to DB case
    nonvar(Local_Object), var(DB_Object), !,
    Ctx = context(object_local_db/3, _),

    arg(1, Local_Object, Local_Class_Id),
    obj_field(Local_Object, db_class, DB_Class_Id0),
    copy_term(DB_Class_Id0, DB_Class_Id),
    (  nonvar(DB_Class_Id)
    -> db_class_des(DB_Key, DB_Class_Id, _, Class, _, Fields, _, _), !
    ;  % get db id (and store service predicates if necessary)
       db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id,
                        Des),
       Des = db_class_des(_, _, Class, _, Fields, _, _)
    ),

    % replace the id
    obj_unify_int(Local_Class_Id, Fields, throw,
		  Local_Object, DB_Field_Vals, Ctx),

    DB_Object =.. [Class, DB_Class_Id | DB_Field_Vals].


object_local_db(DB_Key, Local_Class_Id-Local_Object, DB_Class_Id-DB_Object) :-
    % it is from DB to Local case
    var(Local_Object), nonvar(DB_Object), !,
    Ctx = context(object_local_db/3, _),

    % get local class id
    arg(1, DB_Object, DB_Class_Id),
    db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id,
		     Des),
    class_id(Local_Class_Id, Class),%get the local class name

    Des = db_class_des(_, _, _, _, Fields, _, _),

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
     db_erase_int(DB_Key, Conflicting),
     fail ; true
  ).

%% key_conflict(+DB_Key, +Class_Id, @Object, -Conflicting)
%% is nondet
%
% On BT return all conflicting objects. Some objects can
% be returned more than one due to possible different key
% rules defined in the DB.
%
% This is the rule of Conflicting objects search:
%
% Try all keymasters (KM1) present in DB
% (up to hierarchy from the Object).
% Get the Object key value (KV) based on KM1.
% For each class which is a descendant of KM1 try unify
% objects of this class with KV.

% NB need db <-> local keymasters match
key_conflict(DB_Key, Class_Id, Object, Conflicting) :-

   Ctx = context(key_conflict/4, _),
   %class_id(Class_Id, Class),

   % ensure the info about Class_Id and all its parents
   % are in the DB
   db_conv_local_db(DB_Key, Class_Id, _, _),

   class_path(db_object_v-_, _-Class_Id, _, Path1), !,
   class_path_extract_list(name, Path1, Path2),
   reverse(Path2, Path),
   % BT keymasters up to the hierarchy
   member(Key_Class_Name, Path),
   db_keymaster(DB_Key, Key_Class_Name),

   % get the key of keymaster
   Keymaster_Des = db_class_des(Keymaster_DB_Id, _,
                                Key_Class_Name, _, _, Key,
                                _),
   db_des(DB_Key, Keymaster_Des),
   Key \= [],
   db_conv_local_db(DB_Key, Keymaster_Id, Keymaster_DB_Id,
                    _),

   % get the key value from Object
   obj_unify_int(Class_Id, Key, throw, Object, Key_Value,
                 Ctx),

   % try to unify (copy_term is used to ignore
   % the bind result in Object)
   copy_term_nat(Key_Value, Test_Value),

   % try all descriptors from Keymaster_Id down to the
   % hierarchy
   (  Conflicting_Class_Id = Keymaster_Id
   ;  descendant_class(Conflicting_Class_Id, Keymaster_Id)
   ),
   db_conv_local_db(DB_Key, Conflicting_Class_Id,
                    _, Conflicting_Des),
%   Conflicting_Des = db_class_des(Conflicting_DB_Id, _,
%                                  _, _, _,
%                                  _, _),
%   db_des(DB_Key, Conflicting_Des),
%   db_conv_local_db(DB_Key, Conflicting_Class_Id,
%		    Conflicting_DB_Id, _),

   % the keymaster of the conflicting class should not be
   % descendant of the first class keymaster but not the
   % first class
   get_keymaster(Conflicting_Class_Id,
                 Conflicting_Keymaster_Id),
   \+ (
         % there is keymaster K2 >= Conflicting_Keymaster
         same_or_descendant(Conflicting_Keymaster_Id,
                            K2_Id),
         class_id(K2_Id, K2),
         db_keymaster(DB_Key, K2),

         % K2 < Keymaster
         descendant_class(K2_Id, Keymaster_Id),
         % Class <= K2
         same_or_descendant(Class_Id, K2_Id)
      ),

   named_args_unify_int(DB_Key, fail, Conflicting_Des,
                        Key, Test_Value, Conflicting).



named_args_unify_int(DB_Key, Option, Des, Field_Names, Values,
                     Term) :-

   Ctx = context(named_args_unify_int/6, _),
   Des = db_class_des(DB_Class_Id, _, _, _, _, _, _),
   db_conv_local_db(DB_Key, Local_Class_Id, DB_Class_Id, _),
   % now the class is definitly loaded

   % exclude_field(db_key, Field_Names0, Field_Names1,
   %               Values0, Values1),
   % Field_Names = [db_key|Field_Names1],
   % Values = [DB_Key|Values1],

   copy_term(Values, Values_Templ), % do not unify Values in construct, e.g. class
   obj_construct_int(Local_Class_Id, Field_Names, Option, Values_Templ,
                     Term0),
   (  same_or_descendant(Local_Class_Id, _, db_object_v)
   -> Term = Term0
   ;  obj_rebase((object_v -> db_object_v), Term0, Term)
   ),
   db_recorded_int(DB_Key, Term),

   obj_unify_int(Local_Class_Id, Field_Names, Option, Term, Values, Ctx),
   debug(vd, '~p', named_args_unify_int(DB_Key, Option, Des, Field_Names,
                                        Values, Term)).

% exclude_field(_, [], [], [], []) :- !.

% exclude_field(Field, [Field|Ft0], Ft, [_|Vt0], Vt) :- !,

%    exclude_field(Field, Ft0, Ft, Vt0, Vt).

% exclude_field(Field, [F|Ft0], [F|Ft], [V|Vt0], [V|Vt]) :-

%    exclude_field(Field, Ft0, Ft, Vt0, Vt).












