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

% This is an object database
% The internal prolog DB implementation.

:- module(vd,
          [
%           db_bind_obj/3, % +DB_Key, +Object0, -Object
%           db_change/4,   % +DB_Key, +Fields, +Vals, +Query
           db_clear/1,
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
           db_put_object/4,  % +DB_Key,+Options,+Object0,-Object
           db_put_object/5,  % +DB_Key,+Options,+Object0,-Object,
                             % -Replaced
           
           db_recorda/2,     % +DB_Key, +Object
           db_recorda/3,     % +DB_Key, +Object0, -Object
           db_recorda/4,     % +DB_Key,+Options,+Object0,-Object
           db_recorda/5,     % +DB_Key,+Options,+Object0,-Object,
                             % -Replaced
           
           db_put_objects/3, % +DB_Key, :Pred, +Options

	   db_recorded/2,    % +DB_Key, ?Object
                             % -Object
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

           named_args_unify/5, % +DB_Key, ?Functor, +Field_Names,
                              % ?Values, -Term

           prolog:message//1
           ]).

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/objects_i)).
:- use_module(u(v)).
:- use_module(u(logging)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_terms)).

:- meta_predicate db_iterate(+, +, 1, -).
:- meta_predicate db_iterate_replace(+, 3, +).
:- meta_predicate db_iterate_replace(+, 3, +, 1).
:- meta_predicate db_iterate_replace(+, 3, +, 1, +).
:- meta_predicate db_search(+, +, 1).
:- meta_predicate db_put_objects(+, 1, +).

db_clear(DB_Key) :-

   Ctx = context(db_clear/1, _),
   check_db_key(DB_Key, Ctx),
   db_clear_int(DB_Key).

% db_construct(+DB_Key, +Class, +Fields, +Values)
%
% Construct the object directly in the DB
db_construct(DB_Key, Class, Fields, Values) :-

   Ctx = context(db_construct/4, _),
   db_construct2(DB_Key, Class, Fields, Values, _, Ctx).

% db_construct(+DB_Key, +Class, +Fields, +Values, -Obj)
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


db_copy(DB_In, DB_Out) :-

   Ctx = context(db_copy/2, _),
   (   db_recorded_int(DB_In, Obj0),
       arg(1, Obj0, Class_Id),
       
       obj_rewrite_int(Class_Id, Obj0,
                       [db_key, db_ref], _,
                       [_, _], Obj1, Ctx),
       
       db_put_object_int(DB_Out, Class_Id, _, recordz, Obj1, _,
                         false, Ctx),
       fail
   ;
       true
   ).


db_erase(Obj) :-

   Ctx = context(db_erase/1, _),
   check_object_arg(Obj, Ctx, Class_Id),

   obj_unify_int(Class_Id,
                 [db_key, db_ref], throw, Obj,
                 [DB_Key, DB_Ref], Ctx),

    (   nonvar(DB_Key) -> true
    ;   throw(error(domain_error(bound_db_key, Obj), Ctx))
    ),
    (   nonvar(DB_Ref) -> true
    ;   throw(error(domain_error(bound_db_ref, Obj),Ctx))
    ),

   db_erase_int(DB_Key, Obj).

/*
db_bind_obj(DB_Key, w(Ref0, Object0), w(Ref, Object)) :- !,

   % It is det if Ref0 is bound
   (nonvar(Ref0) -> Is_Det = true ; Is_Det = false),

   duplicate_term(Object0, Object),

   (  db_recorded(DB_Key, Object, Ref0)

      % in the case Ref0 is ground get exactly the same record
   -> (Is_Det == true -> ! ; true),
      db_erase(Ref0),
      db_put_object(DB_Key, w(Ref, Object))
   ).
*/

% db_put_object(+DB_Key, +Object)
%
% Записывает объект в базу, не допуская повторения ключей
% Если ключ содержит свободные поля, они вносят дополнительные
% ограничения, как если бы могли содержать любое значение.
%

db_put_object(DB_Key, Object) :-

   Ctx = context(db_put_object/2, _),
   db_put_object_cmn(DB_Key, _, _, Object, _, false, Ctx).

% db_put_object(+DB_Key, +Object0, -Object)
%
% This version return the object unified with DB

db_put_object(DB_Key, Object0, Object) :-

   Ctx = context(db_put_object/3, _),
   db_put_object_cmn(DB_Key, _, _, Object0, Object, false, Ctx).


% db_put_object(+DB_Key, +Option, +Object0, -Object)
%
% Option: overwrite - удаляет старые объекты по этому
% значению ключа (но только, если все значения ключа связаны)
%          ignore - не добавлять объект, если есть
%          fail - fail predicate on key dup
%          throw - throw exception


db_put_object(DB_Key, Option, Object0, Object) :-

   Ctx = context(db_put_object/4, _),
   db_put_object_cmn(DB_Key, Option, _, Object0, Object, false,
                     Ctx).

% db_put_object(+DB_Key, +Option, +Object0, -Object, -Replaced)
%
% This version unifies the last argument with `replaced'
% if the original object has the same db_ref as already
% existing in the database DB_Key

db_put_object(DB_Key, Option, Object0, Object, Replaced) :-

   Ctx = context(db_put_object/5, _),
   db_put_object_cmn(DB_Key, Option, _, Object0, Object, Replaced,
                     Ctx).

% db_recorda(+DB_Key, +Object)
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
   db_put_object_cmn(DB_Key, Option, _, Object0, Object, Replaced,
                     Ctx).



db_put_object_cmn(DB_Key, Option, Order, Object0, Object,
                  Replaced, Ctx) :-

   check_db_key(DB_Key, Ctx),
   check_inst(Object0, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),

   decode_arg([[_],
               [overwrite],
               [ignore],
               [fail],
               [throw], [throws]],
              Option, Option1, Ctx),

   decode_arg([[recordz, _, z],
               [recorda, a]],
              Order, Order1, Ctx),
   
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

   % Rebase if needed

   % Find db_object_v class id
   class_primary_id(db_object_v, DB_Object_V_Id),

   % this block already binds Object and Replaced
   (  same_or_descendant(DB_Object_V_Id, _, Class_Id0)
   ->
      Class_Id = Class_Id0, % already has a db_object_v ancestor

      % Check the replace case
      obj_rewrite_int(Class_Id, Object0,
                      [db_key, db_ref],
                      [Old_DB_Key, Old_DB_Ref], [DB_Key, _],
                      Object, Ctx),
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
         Object = Object0
      )
      
   ;
      (  Replaced = false -> true
      ;  throw(error(db_obj_replace_protector(
               DB_Key, Replaced, Object0), Ctx))
      ),
      obj_rebase((object_v -> db_object_v), Object0, Object),
      arg(1, Object, Class_Id)
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
      % ... it always has key, hide under key-checking
      % for performance
      db_singleton_hook(DB_Key, Class_Id, Object),
                         
      (  % Check the existence of an object with the same key
         key_conflict(DB_Key, Class_Id, Object, Conflicting)
      ->
         % Make an action which is depended on Option
         handle_key_dup(Option, DB_Key, Class_Id,
                        Conflicting, Object),
         (  Option == overwrite
         -> Continue = true
         ;  Continue = fail)
      ;
         Continue = true
      )
   ;
      Continue = true
   ),

   % Put in db
   (  \+ Continue -> true
   ;
      db_record_int(DB_Key, Order, Object, Ctx)
   ).

db_singleton_hook(DB_Key, Class_Id, DB_Singleton) :-

   functor(DB_Singleton, db_singleton_v, _), !,
   obj_field_int(Class_Id, key_policy, throw, DB_Singleton,
                 Key_Policy, _, _),
   (  var(Key_Policy) -> Key_Policy = throw
   ; true
   ),
   db_key_policy(DB_Key, _, Key_Policy).
   

db_singleton_hook(_, _, _).  % a hook should always succeed

%
% Record all solutions of the Pred.
% It is like findall/3, but use the last arg of Pred
% as a template.
% Use the same Options as db_put_objects.
% When Options = [] will raise exception on key duplicates.
%

db_put_objects(DB_Key, Pred, Options) :-

   call(Pred, Object),
   db_put_object(DB_Key, Options, Object, _),
   fail ; true.

% Low-level db unification.
% Object is var or db_object_v desc.
% Doesn't process inheritance
db_recorded(DB_Key, Object) :-

   (   var(Object) ->  true
   ;
       Ctx = context(db_recorded/2, _),
       check_object_arg(Object, Ctx, _)
   ),
   db_recorded_int(DB_Key, Object).

% db_rewrite(+DB_Key, ?Functor, +Fields, @Old_Vals, +New_Vals)
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
  
  


%
% Leave only matched objects from DB by a search criteria
% Do not unify unbounded fields in DB (which are matched).
%
% filter_on_db(+DB_Key, +Field_Names, +Field_Values) :-
%
filter_on_db(DB_Key, Field_Names, Field_Values) :-

   Ctx = context(filter_on_db/3, _),
   check_inst(Field_Names, Ctx),
   check_inst(Field_Values, Ctx),
   check_db_key(DB_Key, Ctx),
   check_fields_arg(Field_Names, Ctx),
   check_values_arg(Field_Names, Field_Values, Ctx),
   must_be(list(nonvar), Field_Values),

   (   db_recorded_int(DB_Key, Obj),
       arg(1, Obj, Class_Id),
       \+ obj_unify_int(Class_Id, Field_Names, fail, Obj, Field_Values,
                        Ctx),
       db_erase_int(DB_Key, Obj),
       fail
   ;
       true
   ).

% db_search(+DB_In, +DB_Out, :Pred)
%
% Copy from DB_In to DB_Out all filtered by Pred
%
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
         Functor^db_iterate2(people, functor(Functor), O),
         List
        ).

% db_select(+DB_Key, +Fields, ?Row)
%
% BT on all matched rows

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
   db_select_int(DB_Key, Functor, weak, Fields1, Row1, Ctx).


db_select_int(DB_Key, Functor, Weak, Fields, Row, Ctx) :-

   db_functor_des(DB_Key, Functor, Des, Ctx),
   named_args_unify_int(DB_Key, Weak, Des, Fields, Row, _).
       


% db_select_list(+DB_Key, ?Functor, +Fields, -List)
%
% Generate list of lists of selected fields

db_select_list(DB_Key, Functor, Fields, List) :-

   Ctx = context(db_select_list/4, _),
   % <NB> weak is default
   % (to mix class with all descendants in a request)
   db_select_list_cmn(DB_Key, Functor, weak, Fields, List,
                         Ctx). 

% db_select_list(+DB_Key, ?Functor, ?Weak, +Fields, -List)
%

db_select_list(DB_Key, Functor, Weak, Fields, List) :-

   Ctx = context(db_select_list/5, _),
   db_select_list_cmn(DB_Key, Functor, Weak, Fields, List,
                         Ctx). 

db_select_list_cmn(DB_Key, Functor, Weak, Fields, List,
                      Ctx):-

   check_inst(Fields, Ctx),
   check_db_key(DB_Key, Ctx),
   
   decode_arg([[throw, throws, strict, s],
               [weak, _, unbound, w],
               [fail, false, f]],
              Weak, Weak1,
              Ctx),
   
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

%
% DB search
%
% expr ::= expr \/ expr
% expr ::= expr /\ expr
% expr ::= ( expr )
% expr ::= field(Value) | field(+bound) | field(+free) | true
% expr ::= field(\+ Value)
%

%
% On bt return all records selected by Query
%

db_iterate(DB_Key, Query, Object) :-

   Ctx = context(db_iterate/3),
   check_inst(Query, Ctx),
   check_db_key(DB_Key, Ctx),

   db_iterate2(DB_Key, Query, Object).

db_iterate(DB_Key, Query, Filter_Pred, Object) :-

   Ctx = context(db_iterate/4, _),
   check_inst(Query, Ctx),
   check_db_key(DB_Key, Ctx),
   must_be(callable, Filter_Pred),

   db_iterate2(DB_Key, Query, Object),
   ( Filter_Pred = _:true
   -> true
   ; once(call(Filter_Pred, Object))
   ).

db_iterate2(DB_Key, Query, Object) :-
   
   % BT 1
   parse_db_query(DB_Key, Query, Des, Fields, Values),

   % BT 2
   named_args_unify_int(DB_Key, fail, Des, Fields, Values,
                        Object).


%
% Call Pred on all records meat the criteria
% and replace object in DB
%
% For prevent an infinite loop
% Query must reject all new records!
%

db_iterate_replace(DB_Key, Pred, Query) :-

  db_iterate_replace2(DB_Key, Pred, Query, true).


% Limit succesfull replaces by Lim
% (it can speed-up the replacing)

db_iterate_replace(DB_Key, Pred, Query, Filter_Pred) :-

  db_iterate_replace(DB_Key, Pred, Query, Filter_Pred, _).

% Limit succesfull replaces by Lim
% Succesfullnes is defined by the last parameter of Pred

db_iterate_replace(DB_Key, Pred, Query, Filter_Pred, Lim) :-

   (   var(Lim)
   ->  db_iterate_replace2(DB_Key, Pred, Query, Filter_Pred)
   ;   integer(Lim), Lim > 0,
       nb_setval(db_iterate_replace_counter, 0),
       (   db_iterate(DB_Key, Query, Obj_In),
           once(call(Pred, Obj_In, Obj_Out, Is_Succ)),
           db_erase(Obj_In),
           db_put_object(DB_Key, overwrite, Obj_Out, _),
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

parse_db_query(DB_Key, true, Des, [], []) :- !,

   db_des(DB_Key, Des).

%parse_db_query(DB_Key, functor(Class), Des, [], []) :- !,

   % check_class_arg
%   Des = db_class_des(_, _, Class, _, _, _, _),
%   db_des(DB_Key, Des).

parse_db_query(DB_Key, Expr, Des, [Field], [Value]) :-

   functor(Expr, Field, 1), !,
   arg(1, Expr, Value),
   
   %Des = db_class_des(_, _, _, _, DB_Fields, _, _),
   db_des(DB_Key, Des).

   % only those classes which contain Field
   % <NB> no evaluated fields
%   ord_memberchk(Field, DB_Fields).
   

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


% db_name(?DB_Key)
%
% Iterate on over all DBs

db_name(DB_Key) :-

   db_name_int(DB_Key).


% Get a number of recrods

db_size(DB_Key, N) :-

   % TODO improve (memory usage)
   findall('.', db_recorded(DB_Key, _), AL),
   length(AL, N).

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

%
% try unify DB1_Key x DB2_Key with key restrictions,
% leave only unified in DB_Key1,
% always cast to more narrowed type.
% Drop all values with incomplete keys.
% Don't unify if a key is empty (it is a performance restriction).
%
/*
db_merge(DB1_Key, DB2_Key) :-

  db_merge(DB1_Key, DB2_Key, default).


% Merge by Key fields

db_merge(DB1_Key, DB2_Key, Key) :-

   atom(DB1_Key), atom(DB2_Key),
   atom_concat(DB1_Key, '.db_merge', DB_Tmp),
   write_log(['Start db_merge ', DB1_Key, ' and ', DB2_Key],
             [logger(db_merge), lf(1, before), lf(1)]),
   (
   db_recorded(DB1_Key, Object1),
   write_log(['Found', Object1, 'in the first DB'],
             [logger(db_merge), lf(1, before), lf(1)]),
   functor(Object1, Class1, _),
   (  Key = default
   -> get_key(Class1, Key1)
   ;  Key1 = Key
   ),
   named_args_unify(Object1, Key1, Key1_Value),
   (
    ground(Key1_Value),
    write_log(['Got the key value', Key1_Value],
              [logger(db_merge), lf(1)]),
    named_args_unify(DB2_Key, Class2, Key1, Key1_Value,
                     w(Object2_Ref, Object2))
    -> %NB use only the first key-unified object from DB2

    write_log(['Found', Object2, 'in the second DB'],
              [logger(db_merge), lf(1)]),
    most_narrowed(Class1, Class2, New_Class),
    obj_downcast(Object1, New_Class, Final_Obj1),
    obj_downcast(Object2, New_Class, Final_Obj2),

    Final_Obj1 = Final_Obj2,
    db_erase(Object2_Ref),

    db_recordz(DB_Tmp, Final_Obj2),
    write_log([Final_Obj2, 'is written into the first DB'],
              [logger(db_merge), lf(1)])
    ;
    write_log('The corresponding object in the second DB is not found',
              [logger(db_merge), lf(1)])
   ),
   fail
   ;
   true
   ),
   db_clear(DB1_Key),
   db_clear(DB2_Key),
   db_move_all_data(DB_Tmp, DB1_Key).

*/




% named_args_unify(+DB_Key, ?Functor, +Field_Names, ?Values, -Term)
%
% Унификация с расширенной базой данных пролога по полю Field_Name
% и значению Value для тех фактов, которые созданы как классы
%

% this is a version for +Functor
named_args_unify(DB_Key, Functor, Field_Names, Values, Term) :-

   Ctx = context(named_args_unify/5, _),
   check_db_key(DB_Key, Ctx),
   check_fields_arg(Field_Names, Ctx),
   (   var(Functor) -> true
   ;   check_class_arg(Functor, Ctx)
   ),

   list_to_ord_set(Field_Names, Req_Fields),
   (   % BT on all DB Functor values (if it is unbound)
       db_functor_des(DB_Key, Functor, Des, Ctx),

       % check fields compatibility
       Des = db_class_des(_, _, _, _, DB_Fields, _, _),
       ord_subset(Req_Fields, DB_Fields),

       named_args_unify_int(DB_Key, throw, Des, Field_Names,
                            Values, Term)
   ).

db_object_class(DB_Key, Class) :-

   db_object_class_int(DB_Key, Class_Id),
   class_id(Class_Id, Class).


:- initialization clear_decode_arg.
