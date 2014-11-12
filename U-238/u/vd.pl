%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
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
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%  --------------------------------------------------------------


:- module(vd,
          [
%           db_bind_obj/3, % +DB_Key, +Object0, -Object
%           db_change/4,   % +DB_Key, +Fields, +Vals, +Query
           db_clear/1,     % +DB_Key
           db_construct/4, % +DB_Key, +Class, +Fields, +Values
           db_construct/5, % +DB_Key, +Class, +Fields, +Values, -Object
           db_copy/2,
           db_erase/1,
           db_iterate/3,  % +DB_Key, +Query, -Object
           db_iterate/4,  % +DB_Key, +Query, +Filter_Pred, -Object
           db_iterate_replace/3,  % +DB_Key, +Pred, +Query
           db_iterate_replace/4,  % +DB_Key, +Pred, +Query, +Filter
           db_iterate_replace/5,  % +DB_Key, +Pred, +Query,
                                  % +Filter_Pred, +Count

           %db_merge/2,  % by key
           %db_merge/3,  % by custom values
           db_move_all_data/2,
           db_name/1,         % ?DB_Key
           db_object_class/2, % +DB_Key, -Class

           db_put_object/2,  % +DB_Key, +Object
           db_put_object/3,  % +DB_Key, +Object0, -Object
           db_put_object/4,  % +DB_Key,+Option,+Object0,-Object
           db_put_object/5,  % +DB_Key,+Option,+Object0,-Object,
                             % -Replaced

           db_recorda/2,     % +DB_Key, +Object
           db_recorda/3,     % +DB_Key, +Object0, -Object
           db_recorda/4,     % +DB_Key,+Option,+Object0,-Object
           db_recorda/5,     % +DB_Key,+Option,+Object0,-Object,
                             % -Replaced

           db_put_objects/3, % +DB_Key, :Pred, +Options

           db_recorded/2,    % +DB_Key, ?Object
%           db_rewrite/5,     % +DB_Key, ?Functor, +Fields,
                             % @Old_Vals, +New_Vals

           %db_reset/3,       % +DB_Key, +Fields, +Query
           db_search/3,      % +DB_In, +DB_Out, :Pred
           db_size/2,        % +DB_Key, ?Size
           db_to_list/3,     % +DB_Key, ?Functor, -List
           db_select/3,      % +DB_Key, +Fields, ?Row
           db_select_list/4, % +DB_Key, ?Functor, +Fields, -List
           db_select_list/5, % +DB_Key, ?Functor, ?Weak, +Fields,
                             % -List

           dump_db/1,        % +DB_Key
           dump_db/2,        % +Options, +DB_Key
           filter_on_db/3,   % +DB_Key, +Field_Names, +Field_Values
           filter_on_db/4,   % +DB_Key, +Weak, +Field_Names, +Field_Values

           named_args_unify/5, % +DB_Key, ?Functor, +Field_Names,
                              % ?Values, -Term

           prolog:message//1
           ]).

/** <module> Uranium object database

  An object database is a special database for storing uranium
  objects (see v.pl).

  An underlying implementation can be based on any mechanism -
  standard Prolog DB of terms (assertz/1, retract/1), PostgreSQL
  DB etc. Some DBs are _temporary_ (like Prolog DB), others are
  _persistent_ (like PostgreSQL). All temporary DBs are destroyed
  when the program terminates.

  Each database is identified by DB_Key, it is a mandatory
  parameter of any predicate from this module (and usually it is
  the first parameter).

  A database is firstly created when a user puts objects (with
  predicates db_construct/4, db_construct/5, db_copy/2,
  db_move_all_data/2, db_put_object/2, db_put_object/3,
  db_put_object/4, db_put_object/5, db_put_objects/3,
  db_recorda/2, db_recorda/3, db_recorda/4, db_recorda/5) and no
  DB with such DB_Key exists.

  DB_Key can be an atom or a compound term. In the case of atom
  DB_Key a Prolog DB will be used otherwise the used underlying
  module is determined by the DB_Key functor.

  ---++ Storing of class information in every database

  Every Uranium database store information for reconstructing
  stored classes even if the program does not contain definitions
  of stored classes or the program contains different definition
  or the stored classes where rebased some way (see
  obj_rebase/3). It always contains classes for actually stored
  objects as well as they parents (as returned by obj_parents/2,
  i.e., with rebasing).

  ---++ Key conflicts

  Let's make 2 definitions:

   $ keymaster : a class which introduce a new key (it can be []
   - no key also). Can be only db_object_v or its descendant.

   $ keymaster of the class : the class ancestor (or the same
   class) which is a keymaster

   $ nearest common keymaster of the classes: a common
   keymaster of the classes which has no another common keymaster
   of the same classes as a descendant

  When it puts an object into Uranium database it always performs
  a key conflict check. A key conflict is defined as follows:

   1. The class of the new object has a non-empty key.

   1. There is another object in db which can be unified with
   this object by the non-empty key fields of the nearest common
   keymaster of both classes.

  ---++ Key conflicts resolution policy

  When the system finds a key conflict it always resolve it
  according to _|key conflicts resolution policy|_. There are 3
  possible mechanisms by which the policy can be defined:

   1. A default policy for newly created DB. It is always *throw*.

   1. A per-DB policy defined by v/db_properties_v.pl, see.

   1. A per-predicate policy, see db_put_object/4.

  All possible key resolution policies are:

   * overwrite
   Remove the conflicting objects from DB before put new one.

   * ignore
   Ignore the new object.

   * fail
   Fail the predicate, do not put new object.

   * throw
   Throw =|error(db_key_exists(DB_Key, DB_Object, New_Object), _)|=

*/

% Also contains the implementation based on a standard prolog DB

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/class_create)).
:- use_module(u(v)).
:- use_module(u(logging)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_terms)).

:- reexport(u(internal/db_i), [db_key_is_valid/1]).

%% db_clear(+DB_Key) is det.
%
% Clear the DB identified by DB_Key if the DB exists.

db_clear(DB_Key) :-

   debug(vd, '~p', db_clear(DB_Key)),
   Ctx = context(db_clear/1, _),
   check_db_key(DB_Key, Ctx),
   db_clear_int(DB_Key).

%% db_construct(+DB_Key, +Class, +Fields, +Values)
%
% Construct the object directly in the DB
db_construct(DB_Key, Class, Fields, Values) :-

   Ctx = context(db_construct/4, _),
   db_construct2(DB_Key, Class, Fields, Values, _, Ctx).

%% db_construct(+DB_Key, +Class, +Fields, +Values, -Obj)
%
% This form returns the object (a db_object_v descendant)
db_construct(DB_Key, Class, Fields, Values, Obj) :-

   Ctx = context(db_construct/3, _),
   db_construct2(DB_Key, Class, Fields, Values, Obj, Ctx).

db_construct2(DB_Key, Class, Fields, Values, Obj, Ctx) :-

   check_inst(Class, Ctx),
   check_db_key(DB_Key, Ctx),
   check_existing_class_arg(Class, Ctx, Class_Id),
   check_fields_arg(Fields, Ctx),
   check_values_arg(Fields, Values, Ctx),

   obj_construct_int(Class_Id, Fields, throw, Values, Tmp),
   db_put_object_int(DB_Key, Class_Id, _, recordz, Tmp, Obj,
                     false, Ctx).

%% db_copy(+DB_In, +DB_Out) is det.
%
% Copy all objects from DB with the key DB_In to DB with the key
% DB_Out. Do nothing if DB_In is empty or not exists.

db_copy(DB_In, DB_Out) :-

   Ctx = context(db_copy/2, _),
   (   db_recorded_int(DB_In, Obj0),
       arg(1, Obj0, Class_Id),

       obj_rewrite_int(Class_Id, Obj0, throw,
                       [db_key, db_ref, db_class], _,
                       [_, _, _], Obj1, Ctx),

       db_put_object_int(DB_Out, Class_Id, _, recordz, Obj1, _,
                         false, Ctx),
       fail
   ;
       true
   ).

%% db_erase(?Obj) is semidet.
%
% Erase the object Obj from DB. Obj should be Uranium object with
% ground _db_key_, _db_ref_ and _db_class_ fields (usually a
% _db_object_v_ descendant, see db_object_v.pl), i.e., these
% fields idenitify the database and object to be erased.
%
% Fail if Obj is absent in DB.
%
% Bound free Obj fields with the values of the actual DB object
% before erasing.
%
% @error domain_error(bound_db_key, Obj) db_key should be ground
% @error domain_error(bound_db_ref, Obj) db_ref should be ground
% @error domain_error(bound_db_class, Obj) db_class should be ground

db_erase(Obj) :-

   debug(vd, '~p', db_erase(Obj)),
   Ctx = context(db_erase/1, _),
   check_object_arg(Obj, Ctx, Class_Id),

   obj_unify_int(Class_Id,
                 [db_key, db_ref, db_class], throw, Obj,
                 [DB_Key, DB_Ref, DB_Class], Ctx),

    (   ground(DB_Key) -> true
    ;   throw(error(domain_error(bound_db_key, Obj), Ctx))
    ),
    (   ground(DB_Ref) -> true
    ;   throw(error(domain_error(bound_db_ref, Obj),Ctx))
    ),
    (   ground(DB_Class) -> true
    ;   throw(error(domain_error(bound_db_class, Obj),Ctx))
    ),

   db_erase_int(DB_Key, Obj), !.

%% db_put_object(+DB_Key, ?Object) is semidet.
%
% Put Object into DB DB_Key. The order of objects is
% not guaranteed. For ordered put use recorda or recordz.
%
% Usually it is det but can fail if the DB key policy is fail and
% a key conflict occures.

db_put_object(DB_Key, Object) :-

   Ctx = context(db_put_object/2, _),
   db_put_object_cmn(DB_Key, _, _, Object, _, false, Ctx).

%% db_put_object(+DB_Key, ?Object0, -Object) is semidet.
%
% Like db_put_object/2 but also return the object unified with DB
% (it will be always db_object_v descendant with ground =db_key=
% and =db_ref= fields (see db_object_v.pl)).
%
% In the case of =ignore= key policy Object is unified with the
% first conflicting object from DB.

db_put_object(DB_Key, Object0, Object) :-

   Ctx = context(db_put_object/3, _),
   db_put_object_cmn(DB_Key, _, _, Object0, Object, false, Ctx).


%% db_put_object(+DB_Key, +Option, ?Object0, -Object) is semidet.
%
% Like db_put_object/3 but use per-predicate key conflict
% resolution depending on Option (see above).
%

db_put_object(DB_Key, Option, Object0, Object) :-

   Ctx = context(db_put_object/4, _),
   db_put_object_cmn(DB_Key, Option, _, Object0, Object, false,
                     Ctx).

%% db_put_object(+DB_Key, +Option, +Object0, -Object, -Replaced)
%
% This version unifies the last argument with `replaced'
% if the original object has the same db_ref as already
% existing in the database DB_Key

db_put_object(DB_Key, Option, Object0, Object, Replaced) :-

   Ctx = context(db_put_object/5, _),
   db_put_object_cmn(DB_Key, Option, _, Object0, Object, Replaced,
                     Ctx).

%% db_recorda(+DB_Key, +Object)
%
% Put object before others objects of the same class
%
% <NB> the order is warranteed only between objects of the same
% class

db_recorda(DB_Key, Object) :-

   Ctx = context(db_recorda/2, _),
   db_put_object_cmn(DB_Key, _, recorda, Object, _, false, Ctx).

db_recorda(DB_Key, Object0, Object) :-

   Ctx = context(db_recorda/3, _),
   db_put_object_cmn(DB_Key, _, recorda, Object0, Object, false,
                     Ctx).

db_recorda(DB_Key, Option, Object0, Object) :-

   Ctx = context(db_recorda/4, _),
   db_put_object_cmn(DB_Key, Option, recorda, Object0, Object,
                     false, Ctx).

db_recorda(DB_Key, Option, Object0, Object, Replaced) :-

   Ctx = context(db_recorda/5, _),
   db_put_object_cmn(DB_Key, Option, recorda, Object0, Object, Replaced,
                     Ctx).



db_put_object_cmn(DB_Key, Option, Order, Object0, Object,
                  Replaced, Ctx) :-

   Ptx = db_put_object_cmn/7,
   check_db_key(DB_Key, Ctx),
   check_inst(Object0, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),

   decode_arg([[_],
               [overwrite],
               [ignore],
               [fail],
               [throw], [throws]],
              Option, Option1, context(Ptx, 1)),

   decode_arg([[recordz, _, z],
               [recorda, a]],
              Order, Order1, context(Ptx, 2)),

   db_put_object_int(DB_Key, Class_Id, Option1, Order1, Object0,
                     Object, Replaced, Ctx).


handle_key_dup(Option, DB_Key, Class_Id, DB_Object, New_Object) :-

   var(Option), !,

   db_key_policy(DB_Key, Option, _),
   % Options is always bound after the call

   handle_key_dup(Option, DB_Key, Class_Id, DB_Object,
                  New_Object).

handle_key_dup(throw, DB_Key, _, DB_Object, New_Object) :-
   throw(error(db_key_exists(DB_Key, DB_Object, New_Object), _)).
handle_key_dup(fail, _, _, _, _) :- !, fail.
handle_key_dup(ignore, _, _, _, _) :- !.
handle_key_dup(overwrite, DB_Key, Class_Id, _, New_Object) :-
   erase_conflicts(DB_Key, Class_Id, New_Object), !.


%db_put_object_int(+DB_Key, +Class_Id0, ?Option, ?Order,
%                  +Object0, -Object, -Replaced, +Ctx)
db_put_object_int(DB_Key, Class_Id0, Option, Order, Object0,
                  Object, Replaced, Ctx) :-

   debug(vd, '~p', db_put_object_int(DB_Key, Class_Id0, Option,
                                     Order, Object0, Object, Replaced,
                                     Ctx)),

   % Rebase if needed

   % this block already binds Object1 and Replaced
   (  is_db_ready(Object0)
   ->
      Class_Id = Class_Id0, % already has a db_object_v ancestor

      % Check the replace case
      obj_rewrite_int(Class_Id, Object0, throw,
                      [db_key, db_ref],
                      [Old_DB_Key, Old_DB_Ref], [DB_Key, _],
                      Object1, Ctx),
      (  ground(Old_DB_Ref),
         Old_DB_Key = DB_Key
      ->
         % it is replacing, try unify the protector
         (
            Replaced = replaced
         -> true % the replacing is allowed
         ;  throw(error(db_obj_replace_protector(DB_Key,
                  Replaced, Object0), Ctx))
         )

      ;  ground(Old_DB_Ref)
      -> throw(error(domain_error(unbound_db_ref, Old_DB_Ref),
                     Ctx))

      ;  Old_DB_Key \= DB_Key
      -> throw(error(domain_error(unbound_or_same_db_key,
                                  Old_DB_Key), Ctx))
      ;
         (  Replaced = false -> true
         ;  throw(error(db_obj_replace_protector(
                  DB_Key, Replaced, Object0), Ctx))
         ),
         Object1 = Object0
      )

   ;
      (  Replaced = false -> true
      ;  throw(error(db_obj_replace_protector(
               DB_Key, Replaced, Object0), Ctx))
      ),
      make_db_ready(DB_Key, Object0, Object1, Ctx),
      arg(1, Object1, Class_Id)
   ),

   % Remove the old object first (before the key conflicts check)
   (  Replaced == replaced
   -> db_erase_int(DB_Key, Object0)
   ;  true
   ),

   % Check the key if any
   get_key(Class_Id, Key),
   (  Key \== []
   ->
      % ... it always has a key, hide under key-checking
      % for performance
      db_properties_hook(DB_Key, Class_Id, Object1),

      (  % Check the existence of an object with the same key
         key_conflict(DB_Key, Class_Id, Object1, Conflicting)
      ->
         % Make an action which is depended on Option
         handle_key_dup(Option, DB_Key, Class_Id,
                        Conflicting, Object1),
         (  Option == overwrite
         -> Continue = true, Object = Object1
         ;  Option == ignore
         -> Continue = fail, Object = Conflicting
         ;  Continue = fail
         )
      ;
         Continue = true, Object = Object1
      )
   ;
      Continue = true, Object = Object1
   ),

   % Put in db
   (  \+ Continue -> true
   ;
      db_record_int(DB_Key, Order, Object, Ctx)
   ).

is_db_ready(Object) :-
   arg(1, Object, Class_Id),
   class_path(db_object_v-_, _-Class_Id, _, _), !.

make_db_ready(DB_Key, Object0, Object, Ctx) :-
   arg(1, Object0, Class_Id),
   list_inheritance_names(Class_Id, Parents0),
   (  Parents0 = [object_base_v, object_v|Parents1]
   -> true
   ;  throw(error(db_unformatted_object(DB_Key, Parents0), Ctx))
   ),
   (  Parents1 = [db_object_v|_]
   -> Object = Object0
   ;
      (  select(db_object_v, Parents1, Parents2) -> true
      ;	 Parents2 = Parents1
      ),
      Parents3 = [object_base_v, object_v, db_object_v | Parents2],
      reverse(Parents3, Parents),
      obj_parents_int(Object0, Parents, Object, Ctx)
   ).



db_properties_hook(DB_Key, Class_Id, DB_Properties) :-

   Ctx = context(db_properties_hook/3, _),
   functor(DB_Properties, db_properties_v, _), !,
   obj_unify_int(Class_Id, [key_policy, after_put_callback], throw,
                 DB_Properties, [Key_Policy, After_Put_Callback], Ctx),
   (  var(Key_Policy) -> Key_Policy = throw
   ; true
   ),
   db_key_policy(DB_Key, _, Key_Policy),

   (  var(After_Put_Callback) -> true
   ;  db_set_callbacks(DB_Key, After_Put_Callback)
   ).


db_properties_hook(_, _, _).  % a hook should always succeed

%% db_put_objects(+DB_Key, :Pred, +Options)
%
% Record all solutions of the Pred.
% It is like findall/3, but use the last arg of Pred
% as a template.
% Use the same Options as db_put_objects.
% When Options = [] will raise exception on key duplicates.
%

:- meta_predicate db_put_objects(+, 1, +).

db_put_objects(DB_Key, Pred, Options) :-

   call(Pred, Object),
   db_put_object(DB_Key, Options, Object, _),
   fail ; true.

%% db_recorded(+DB_Key, ?Object)
%
% Low-level db unification.
% Object is var or db_object_v desc.
% Doesn't process inheritance
db_recorded(DB_Key, Object) :-

   (   var(Object) ->  true
   ;
       Ctx = context(db_recorded/2, _),
       check_object_arg(Object, Ctx, _)
   ),
   db_recorded_int(DB_Key, Object),
   debug(vd, '~p', db_recorded(DB_Key, Object)).

%% db_rewrite(+DB_Key, ?Functor, +Fields, @Old_Vals, +New_Vals)
%
% Change fields in all objects unified with Old_Vals

% db_rewrite(DB_Key, Functor, Fields, Old_Vals, New_Vals) :-

%   Ctx = context(db_rewrite/4),
%   check_inst(Fields, Ctx),
%   check_inst(New_Vals, Ctx),
%   check_db_key(DB_Key, Ctx),
%   check_fields_arg(Fields, Ctx),
%   (   var(Old_Vals) -> true
%   ;   check_values_arg(Fields, Old_Vals, Ctx)
%   ),
%   check_values_arg(Fields, New_Vals, Ctx),

%   db_rewrite_int(DB_Key, Functor, Fields, Old_Vals, New_Vals,
%                  Ctx).

% db_rewrite_int(DB_Key, Functor, Fields, Old_Vals, New_Vals,
%                Ctx) :-

%    db_functor_des(DB_Key, Functor, Des, Ctx),

%    (   named_args_unify_int(DB_Key, throw, Des, Fields, Old_Vals,
%                             Obj0),
%        arg(1, Obj0, Class_Id),

%        obj_rewrite_int(Class_Id, Obj0, Fields, Old_Vals, New_Vals,
%                        Obj1, Ctx),
%        db_put_object_int(DB_Key, Class_Id, _, recordz, Obj1, _,
%                          replaced, Ctx),
%        fail ; true
%    ).




%% filter_on_db(+DB_Key, +Field_Names, @Field_Values) is
%% det.
%
% Leave only matched objects from DB by the search
% criteria Do not unify unbounded fields in DB (which are
% matched).
%
% @see filter_on_db/4

filter_on_db(DB_Key, Field_Names, Field_Values) :-

   Ctx = context(filter_on_db/3, _),
   filter_on_db_cmn(DB_Key, fail, Field_Names,
                    Field_Values, Ctx).

%% filter_on_db(+DB_Key, +Weak, +Field_Names,
%% @Field_Values) is det.
%
% It is like filter_on_db/3 but use the Weak option for
% unmatched fields.
%
% @see filter_on_db/3

filter_on_db(DB_Key, Weak, Field_Names, Field_Values) :-

   Ctx = context(filter_on_db/4, _),
   filter_on_db_cmn(DB_Key, Weak, Field_Names,
                    Field_Values, Ctx).

filter_on_db_cmn(DB_Key, Weak, Field_Names, Field_Values,
                 Ctx) :-

   check_inst(Field_Names, Ctx),
   check_inst(Field_Values, Ctx),
   check_db_key(DB_Key, Ctx),
   check_fields_arg(Field_Names, Ctx),
   check_values_arg(Field_Names, Field_Values, Ctx),
   must_be(list(nonvar), Field_Values),

   (   db_recorded_int(DB_Key, Obj),
       arg(1, Obj, Class_Id),
       \+ obj_unify_int(Class_Id, Field_Names, Weak, Obj,
                        Field_Values, Ctx),
       db_erase_int(DB_Key, Obj),
       fail
   ;
       true
   ).

%% db_search(+DB_In, +DB_Out, :Pred)
%
% Copy from DB_In to DB_Out all filtered by Pred

:- meta_predicate db_search(+, +, 1).

db_search(DB_In, DB_Out, Pred) :-

   Ctx = context(db_search/3, _),
   check_db_key(DB_In, Ctx),
   check_db_key(DB_Out, Ctx),
   must_be(callable, Pred),

   (   db_recorded(DB_In, Term),
       once(call(Pred, Term)),
       obj_reset_fields([db_ref, db_key], Term, Term1),
       db_put_object(DB_Out, Term1),
       fail
   ;
       true
   ).

dump_db(DB_Key) :- dump_db([logger(dump_db)], DB_Key).

dump_db(Options, DB_Key) :-

  ground(DB_Key),

  check_logger(Options) ->
  (
   write_log(['Dump the', DB_Key, 'DB:'],
             [logger(dump_db), lf(1, before), lf(1)]),
   open_log(Options),
   db_recorded(DB_Key, Object),
   log_piece(Object, Options),
   fail
  ;
   close_log(Options)
  )
  ; true.

%
% db_to_list(+DB_Key, ?Functor, -List)
%
% TODO rewrite with db_select_list usage

db_to_list(DB_Key, Functor, List) :-

   Ctx = context(db_to_list/3, _),
   check_db_key(DB_Key, Ctx),
   (  var(Functor) -> true
   ;  check_class_arg(Functor, Ctx)
   ),

   bagof(O,
         Functor^db_iterate2(DB_Key, functor(Functor), O, Ctx),
         List
        ).

%% db_select(+DB_Key, +Fields, ?Row) is nondet.
%
% BT on all matched rows in DB.
%
% @param Fields list of field names; use _weak_ matching (see v.pl).
% @param Row list field values
%
% @see db_select_list/4
% @see db_select_list/5

db_select(DB_Key, Fields, Row) :-

   Ctx = context(db_select/3, _),
   check_inst(Fields, Ctx),
   check_db_key(DB_Key, Ctx),
   check_fields_arg(Fields, Ctx),
   (   var(Row) -> true
   ;   check_values_partlist_arg(Fields, Row, Ctx)
   ),

   (   select_value(functor, Fields, Fields1, Row, Row1, Functor)
   ->  true
   ;   Fields1 = Fields, Row1 = Row
   ),
   db_select_int(DB_Key, Functor, unbound, Fields1, Row1, Ctx).


db_select_int(DB_Key, Functor, Weak, Fields, Row, Ctx) :-

   db_functor_des(DB_Key, Functor, Des, Ctx),
   named_args_unify_int(DB_Key, Weak, Des, Fields, Row, _).



%% db_select_list(+DB_Key, ?Functor, +Fields, -List) is det.
%
%  It is like db_select/3 but return all rows by one call as a
%  list of lists.
%
%  @param Functor if bound then must be a class name; in this
%  case return result only for objects with Functor; return
%  result for all objects in DB in other case.
%
%  @see db_select/3
%  @see db_select_list/5

db_select_list(DB_Key, Functor, Fields, List) :-

   Ctx = context(db_select_list/4, _),
   % <NB> weak is default
   % (to mix class with all descendants in a request)
   db_select_list_cmn(DB_Key, Functor, weak, Fields, List,
                         Ctx).

%% db_select_list(+DB_Key, ?Functor, ?Weak, +Fields, -List) is semidet.
%
% The same as db_select_list/4 but use defined field matching
% rule (see v.pl)
%
% @param Weak
%  * throw, throws, strict, s
%  In a case of absent Field throw an exception
%  * weak, _, unbound, w
%  Leave unbound
%  * fail, false, f
%  Fail the whole predicate
%
% @see db_select/3
% @see db_select_list/4

db_select_list(DB_Key, Functor, Weak, Fields, List) :-

   Ctx = context(db_select_list/5, _),
   db_select_list_cmn(DB_Key, Functor, Weak, Fields, List,
                         Ctx).

db_select_list_cmn(DB_Key, Functor, Weak, Fields, List,
                      Ctx):-

   check_inst(Fields, Ctx),
   check_db_key(DB_Key, Ctx),

   Self_Ctx = context(db_select_list_cmn/6, _),
   decode_arg([[throw, throws, strict, s],
               [unbound, _, weak, w],
               [fail, false, f]],
              Weak, Weak1,
              Self_Ctx),

   (   var(Functor) -> true
   ;   check_class_arg(Functor, Ctx)
   ),
   check_fields_arg(Fields, Ctx),

   db_select_list_int(DB_Key, Functor, Weak1, Fields, List, Ctx).


db_select_list_int(DB_Key, Functor, Weak, Fields, List, Ctx) :-

   findall(Row,
           db_select_int(DB_Key, Functor, Weak, Fields, Row, Ctx),
           List
          ).

%%	db_iterate(+DB_Key, +Query, -Object) is nondet.
%
% On BT return all objects from db DB_Key selected by Query.
% Query is defined by expr.
% ==
% expr ::= expr \/ expr
% expr ::= expr /\ expr
% expr ::= ( expr )
% expr ::= field(Value) | field(+bound) | field(+free) | true
% expr ::= field(\+ Value)
% ==

db_iterate(DB_Key, Query, Object) :-

   Ctx = context(db_iterate/3),
   check_inst(Query, Ctx),
   check_db_key(DB_Key, Ctx),

   db_iterate2(DB_Key, Query, Object, Ctx).

%%	db_iterate(+DB_Key, +Query, :Filter_Pred, -Object) is nondet.
%
% The same as db_iterate/3 but additional filter all objects through
% Filter_Pred. Ignore objects for which Filter_Pred evaluates to false.

:- meta_predicate db_iterate(+, +, 1, -).

db_iterate(DB_Key, Query, Filter_Pred, Object) :-

   Ctx = context(db_iterate/4, _),
   check_inst(Query, Ctx),
   check_db_key(DB_Key, Ctx),
   must_be(callable, Filter_Pred),

   db_iterate2(DB_Key, Query, Object, Ctx),
   ( Filter_Pred = _:true
   -> true
   ; once(call(Filter_Pred, Object))
   ).

db_iterate2(DB_Key, Query, Object, Ctx) :-

   % BT 1
   parse_db_query(DB_Key, Query, Des, Fields, [], Values0, [], Ctx),

   split_bound_unbound(Values0, Values1, Bound, Unbound, Bounding_Check),

   % BT 2
   named_args_unify_int(DB_Key, fail, Des, Fields, Values1,
                        Object0),

   (  Bounding_Check == true
   -> % reload fresh object
      arg(1, Object0, Class_Id),
      obj_field_int(Class_Id, db_ref, throw, Object0, DB_Ref, _, Ctx),
      named_args_unify_int(DB_Key, throw, Des, [db_ref], [DB_Ref], Object),
      obj_unify_int(Class_Id, Fields, throw, Object, Values, Ctx),
      check_bound_unbound(Values, Bound, Unbound)
   ;  Object = Object0
   ).

split_bound_unbound([], [], [], [], _) :- !.
split_bound_unbound([Var|TV0], [Var|TV], [false|TB], [false|TU], BC) :-
   var(Var), !,
   split_bound_unbound(TV0, TV, TB, TU, BC).
split_bound_unbound([+bound|TV0], [_|TV], [true|TB], [false|TU], true) :-
   !, split_bound_unbound(TV0, TV, TB, TU, _).
split_bound_unbound([+free|TV0], [_|TV], [false|TB], [true|TU], true) :-
   !, split_bound_unbound(TV0, TV, TB, TU, _).
split_bound_unbound([Value|TV0], [Value|TV], [true|TB], [false|TU], true) :-
   split_bound_unbound(TV0, TV, TB, TU, _).

check_bound_unbound([], [], []) :- !.
check_bound_unbound([Value|TV], [Bound|TB], [Unbound|TU]) :-
   (  Bound = true -> nonvar(Value) ; true ),
   (  Unbound = true -> var(Value) ; true ),
   check_bound_unbound(TV, TB, TU).


%% db_iterate_replace(+DB_Key, :Pred, +Query)
%
% Call Pred(+Old_Obj, -New_Obj, -Is_Replaced) on all records meet the
% criteria and replace objects in DB. Is_Replaced is for count records
% only (Is_Replaced is analized only in db_iterate_replace/5 form). The
% mode for adding new objects is =overwrite=.
%
% For prevent an infinite loop
% Query must reject all new records!
%

:- meta_predicate db_iterate_replace(+, 3, +).

db_iterate_replace(DB_Key, Pred, Query) :-

  db_iterate_replace2(DB_Key, Pred, Query, true).


%% db_iterate_replace(+DB_Key, :Pred, +Query, :Filter_Pred)
%
% The same as db_iterate_replace/3 but also use Filter_Pred (see
% db_iterate/4).

:- meta_predicate db_iterate_replace(+, 3, +, 1).

db_iterate_replace(DB_Key, Pred, Query, Filter_Pred) :-

  db_iterate_replace(DB_Key, Pred, Query, Filter_Pred, _).

% it's a trick to fight a warning
true(_).

%%	db_iterate_replace(DB_Key, Pred, Query, Filter_Pred, Lim)
%
% Limit succesfull replaces by Lim.
% Succesfullnes is defined by the last parameter of Pred (see
% db_iterate_replace/3). The Lim is for purpose of speed-up the
% replacing, because all new records will be at the end when using
% prolog db, and no need to filter new objects in Query.

:- meta_predicate db_iterate_replace(+, 3, +, 1, +).

db_iterate_replace(DB_Key, Pred, Query, Filter_Pred, Lim) :-

   (   var(Lim)
   ->  db_iterate_replace2(DB_Key, Pred, Query, Filter_Pred)
   ;   integer(Lim), Lim > 0,
       nb_setval(db_iterate_replace_counter, 0),
       (   db_iterate(DB_Key, Query, Obj_In),
           once(call(Pred, Obj_In, Obj_Out, Is_Succ)),
           db_erase(Obj_In), %! do not use db_erase here, use replacing
           db_put_object(DB_Key, overwrite, Obj_Out, _), %! do not imply overwrite policy
           (  Is_Succ
           -> nb_getval(db_iterate_replace_counter, Cnt),
	      succ(Cnt, Cnt1),
	      nb_setval(db_iterate_replace_counter, Cnt1),
	      Cnt1 = Lim, !
           )
       ;
           true
       )
   ).

db_iterate_replace2(DB_Key, Pred, Query, Filter_Pred) :-

   db_iterate(DB_Key, Query, Filter_Pred, Obj_In),
   once(call(Pred, Obj_In, Obj_Out, _)),
   db_erase(Obj_In),
   db_put_object(DB_Key, overwrite, Obj_Out, _),
   fail
   ;
   true.

parse_db_query(DB_Key, true, Des, Fs, Fs, Vs, Vs, _) :- !,

   db_des(DB_Key, Des).

parse_db_query(DB_Key, functor(Functor), Des, Fs, Fs, Vs, Vs, Ctx) :- !,

   (  var(Functor) -> true
   ;  check_class_arg(Functor, Ctx)
   ),
   db_functor_des(DB_Key, Functor, Des, Ctx).

parse_db_query(DB_Key, same_or_descendant(Class), Des,
               Fs, Fs, Vs, Vs, Ctx) :- !,

   check_inst(Class, Ctx),
   check_existing_class_arg(Class, Ctx),
   db_des(DB_Key, Des),
   Des = db_class_des(DB_Class_Id, _, _, _, _, _, _),
   % Des = db_class_des(DB_Class_Id, _, Functor, _, _, _, _),
   % Functor \= Class, % exclude the "same" case
   db_conv_local_db(DB_Key, Desc_Id, DB_Class_Id, Des),
   same_or_descendant(Desc_Id, _, Class). % filter descendants
                                          % (inc. rebased)

parse_db_query(DB_Key, Expr, Des, [Field|Fs], Fs, [Value|Vs], Vs, _) :-

   functor(Expr, Field, 1), !,
   arg(1, Expr, Value),

   db_des(DB_Key, Des).

parse_db_query(DB_Key, Expr1 /\ Expr2, Des, Fs0, Fs, Vs0, Vs, Ctx) :-
   !,
   parse_db_query(DB_Key, Expr1, Des, Fs0, Fs1, Vs0, Vs1, Ctx),
   parse_db_query(DB_Key, Expr2, Des, Fs1, Fs, Vs1, Vs, Ctx).

parse_db_query(DB_Key, Expr1 \/ Expr2, Des, Fs0, Fs, Vs0, Vs, Ctx) :-
   !,
   (  parse_db_query(DB_Key, Expr1, Des, Fs0, Fs, Vs0, Vs, Ctx)
   ;  parse_db_query(DB_Key, Expr2, Des, Fs0, Fs, Vs0, Vs, Ctx)
   ).

parse_db_query(DB_Key, Expr, _, _, _, _, _, Ctx) :-

   throw(error(db_invalid_query(DB_Key, Expr), Ctx)).

db_move_all_data(From_DB, To_DB) :-

   Ctx = context(db_move_all_data/2, _),
   check_db_key(From_DB, Ctx),
   check_db_key(To_DB, Ctx),

   (   db_recorded_int(From_DB, Record),
       arg(1, Record, Class_Id),
       db_put_object_int(To_DB, Class_Id, _, recordz, Record, _,
                         false, Ctx),
       %obj_field_int(Class_Id, db_ref, throw, Record, DB_Ref,
       %              _, Ctx),
       db_erase_int(From_DB, Record),
       fail
   ;
       true
   ).


%% db_name(?DB_Key) is nondet.
%
% When DB_Key is bound this predicate checks whether DB
% exists. In other case it iterates through all databases used by
% the program (of any type).

db_name(DB_Key) :-

   db_name_int(DB_Key).


%% db_size(+DB_Key, ?N)
% Get a number of recrods

db_size(DB_Key, N) :-

   % TODO improve (memory usage)
   findall('.', db_recorded(DB_Key, _), AL),
   length(AL, N).

%% db_change(+DB_Key, +Fields, +Vals, +Query) is semidet.
%
% Change (reset+unify) fields in DB
% is det.
%
/*
db_change(DB_Key, Fields, Vals, Query) :-

   is_list(Vals), ground(Vals), % TODO performance

   % Unification with new vals is able iff the old is reseted
   db_reset(DB_Key, Fields, Query),

   % FIXME it can hangup if all elements of Vals are unbound
   single_expr_list(Fields, +free, List2),
   and_list_query(List2, Set_Query),

   Replace_Query = Query /\ Set_Query,

   db_iterate_replace(DB_Key,
                      unify_fields(Fields, Vals),
                      Replace_Query).


unify_fields(Fields, Vals, Object, Object, true) :-

    named_args_unify(Object, Fields, Vals).


%% db_reset(+DB_Key, +Reset_List, +Query)
%
% Reset fields in DB
% Is det.
%

db_reset(DB_Key, Reset_List, Query) :-

   single_expr_list(Reset_List, +bound, List2),
   or_list_query(List2, Reset_Query),

   Replace_Query = Query /\ Reset_Query,

   db_iterate_replace(DB_Key,
                      obj_reset_fields(Reset_List),
                      Replace_Query).

%
% Convert [field1, field2, ...] <->
%         [field1(Expr), field2(Expr), ...]
%

single_expr_list([], _, []) :- !.

single_expr_list([Head | Tail], Expr, [QHead | QTail]) :-

   QHead =.. [Head, Expr],
   single_expr_list(Tail, Expr, QTail).

%
% Convert [expr1, expr2, ...] <-> (expr1) \/ (expr2) \/ ...
%

or_list_query([Head], Head) :- !.

or_list_query([Head | Tail], Head \/ QTail) :-

   or_list_query(Tail, QTail).

%
% Convert [expr1, expr2, ...] <-> (expr1) /\ (expr2) /\ ...
%

and_list_query([Head], Head) :- !.

and_list_query([Head | Tail], Head /\ QTail) :-

   or_list_query(Tail, QTail).

%
% DB search
%
% expr ::= expr \/ expr
% expr ::= expr /\ expr
% expr ::= ( expr )
% expr ::= field(Value) | field(+bound) | field(+free) | true
% expr ::= field(\+ Value)
% expr ::= functor(Functor) | functor(\+ Functor)
%

check_record(true, _) :- !.

check_record(bound(Num), Record) :-

   !, arg(Num, Record, Value), !,
   ground(Value).

check_record(free(Num), Record) :-

   !, arg(Num, Record, Value), !,
   var(Value).

check_record(value(Num, Val), Record) :-

   !, arg(Num, Record, Value), !,
   ground(Value),
   Val = Value.

check_record(not_value(Num, Val), Record) :-

   !, arg(Num, Record, Value), !,
   ground(Value),
   Val \= Value.

check_record(functor(Functor), Record) :-

   !, functor(Record, Functor, _).

check_record(not_functor(Functor), Record) :-

   !, \+ functor(Record, Functor, _).

check_record(/\(Expr1, Expr2), Record) :-

   !, check_record(Expr1, Record),
   check_record(Expr2, Record), !.

check_record(\/(Expr1, Expr2), Record) :-

   !,
   (   check_record(Expr1, Record)
   ;   check_record(Expr2, Record)
   ), !.
*/

%% db_merge(+DB1_Key, +DB2_Key)
%
% Try to unify DB1_Key x DB2_Key with key restrictions,
% leave only unified in DB_Key1,
% always cast to more narrowed type.
% Drop all values with incomplete keys.
% Don't unify if a key is empty (it is a performance restriction).
%
db_merge(DB1_Key, DB2_Key) :-
   Ctx = context(db_merge/2, _),
   db_merge_cmn(DB1_Key, DB2_Key, default, Ctx).

%% db_merge(+DB1_Key, +DB2_Key, +Key)
% Merge by Key fields

db_merge(DB1_Key, DB2_Key, Key) :-
   Ctx = context(db_merge/3, _),
   db_merge_cmn(DB1_Key, DB2_Key, Key, Ctx).

db_merge_cmn(DB1_Key, DB2_Key, Key, Ctx) :-
   check_db_key(DB1_Key, Ctx),
   check_db_key(DB2_Key, Ctx),
   atom_concat(DB1_Key, '.#db_merge', DB_Tmp),

   write_log(['Start db_merge ', DB1_Key, ' and ', DB2_Key],
             [logger(db_merge), lf(1, before), lf(1)]),

   (  db_recorded_int(DB1_Key, Object1),

      write_log(['Found', Object1, 'in the first DB'],
                [logger(db_merge), lf(1, before), lf(1)]),

      functor(Object1, Class1, _),
      (  Key = default
      -> get_key(Class1, Key1)
      ;  Key1 = Key
      ),
      arg(1, Object1, Class1_Id),
      obj_unify_int(Class1_Id, Key1, throw, Object1, Key1_Value, Ctx),
      (
         ground(Key1_Value),

         write_log(['Got the key value', Key1_Value],
                   [logger(db_merge), lf(1)]),

         named_args_unify(DB2_Key, Class2, Key1, Key1_Value, Object2)
      -> %NB use only the first key-unified object from DB2

         write_log(['Found', Object2, 'in the second DB'],
                   [logger(db_merge), lf(1)]),
         most_narrowed(Class1, Class2, New_Class), %- need check with rebasing
         arg(1, Object2, _),
         obj_downcast_int(Object1, New_Class, Final_Obj1),
         obj_downcast_int(Object2, New_Class, Final_Obj2),

         Final_Obj1 = Final_Obj2,
         db_erase(Object2),

         db_recordz(DB_Tmp, Final_Obj2),
         write_log([Final_Obj2, 'is written into the first DB'],
                   [logger(db_merge), lf(1)])
      ;
         write_log('The corresponding object in the second DB is not found',
                   [logger(db_merge), lf(1)])
      ),
      fail ; true
   ),
   db_clear(DB1_Key),
   db_clear(DB2_Key),
   db_move_all_data(DB_Tmp, DB1_Key).


%% named_args_unify(+DB_Key, ?Functor, +Field_Names, ?Values,
%% -Obj) is nondet.
%
%  Unify Obj with all matched objects from DB_Key with Functor
%  and Field_Names (list) unified with Values (list of the same
%  size). If Functor is unbound bound it with the functor of Obj.

% this is a version for +Functor
named_args_unify(DB_Key, Functor, Field_Names, Values, Term) :-

   Ctx = context(named_args_unify/5, _),
   check_db_key(DB_Key, Ctx),
   check_fields_arg(Field_Names, Ctx),
   (   var(Functor) -> true
   ;   check_class_arg(Functor, Ctx)
   ),

   % BT on all DB Functor values (if it is unbound)
   db_functor_des(DB_Key, Functor, Des, Ctx),

   named_args_unify_int(DB_Key, fail, Des, Field_Names,
                        Values, Term).


db_object_class(DB_Key, Class) :-

   db_object_class_int(DB_Key, Class_Id),
   class_id(Class_Id, Class).


:- initialization clear_decode_arg.
