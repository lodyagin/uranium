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


:- module(vd,
          [
%           db_bind_obj/3, % +DB_Key, +Object0, -Object
%           db_change/4,   % +DB_Key, +Fields, +Vals, +Query
           db_clear/1,
           %db_copy/2,
           %db_iterate/3,  % +DB_Key, +Query, -Object
           %db_iterate/4,  % +DB_Key, +Query, +Filter_Pred, -Object
           %db_iterate_replace/3,  % +DB_Key, +Pred, +Query
           %db_iterate_replace/4,  % +DB_Key, +Pred, +Query, +Filter
           %db_iterate_replace/5,  % +DB_Key, +Pred, +Query, +Filter_Pred, +Count
           %db_merge/2,  % by key
           %db_merge/3,  % by custom values
           db_move_all_data/2,
           %db_object_class/2,
           db_put_object/3,  % +DB_Key, +Object0, -Object
           db_put_object/4,  % +DB_Key,+Options,+Object0,-Object
           db_put_objects/3, % +DB_Key, :Pred, +Options
           %db_reset/3,    % +DB_Key, +Fields, +Query
           %db_search/3,
           db_size/2,        % +DB_Key, ?Size
           %db_to_list/3,
           dump_db/1,  % +DB_Key
           dump_db/2   % +Options, +DB_Key
%           filter_on_db/3
           ]).

:- use_module(u(internal/check_arg)).
:- use_module(u(internal/db_i)).
:- use_module(u(internal/objects_i)).
:- use_module(u(v)).
:- use_module(library(lists)).
:- use_module(u(logging)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_terms)).

:- module_transparent db_put_objects/3, db_search/3.
%                      db_iterate/3, db_iterate/4,
%                      db_iterate_replace/3, db_iterate_replace/4,
%                      db_iterate_replace/5,
%                      db_iterate_replace2/4.

db_clear(DB_Key) :-

   Ctx = context(db_clear/1, _),
   check_db_key(DB_Key, Ctx),
   db_clear_int(DB_Key).
   
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

% db_put_object(+DB_Key, +Object0, -Object)
%
% Записывает объект в базу, не допуская повторения ключей
% Если ключ содержит свободные поля, они вносят дополнительные
% ограничения, как если бы могли содержать любое значение.
%

db_put_object(DB_Key, Object0, Object) :-

   Ctx = context(db_put_object/3, _),
   (  var(Object0)
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_db_key(DB_Key, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),

   db_put_object_int(DB_Key, Class_Id, throw, Object0, Object).


% db_put_object(+DB_Key, +Option, +Object0, -Object)
%
% Option: overwrite - удаляет старые объекты по этому
% значению ключа (но только, если все значения ключа связаны)
%          ignore - не добавлять объект, если есть
%          fail - fail predicate on key dup
%          throw - throw exception


db_put_object(DB_Key, Option, Object0, Object) :-

   Ctx = context(db_put_object/4, _),
   (  (var(Option); var(Object0))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   (  \+ atom(Option)
   -> throw(error(type_error(atom, Option), Ctx))
   ;  (  Option = overwrite
      ;  Option = ignore
      ;  Option = fail
      ;  Option = throw )
   -> true
   ;  throw(error(domain_error(valid_option_value, Option), Ctx))
   ),

   check_db_key(DB_Key, Ctx),
   check_object_arg(Object0, Ctx, Class_Id),

   db_put_object_int(DB_Key, Class_Id, Option,
                     Object0, Object).


handle_key_dup(throw, DB_Key, _, DB_Object, New_Object) :-
   throw(db_key_exists(DB_Key, DB_Object, New_Object)).
handle_key_dup(fail, _, _, _, _) :- !, fail.
handle_key_dup(ignore, _, _, _, _) :- !.
handle_key_dup(overwrite, DB_Key, Class_Id, _, New_Object) :-
   erase_conflicts(DB_Key, Class_Id, New_Object), !.


db_put_object_int(DB_Key, Class_Id, Option, Object0, Object) :-

   % TODO replacing object when db_ref is already bound

   % Rebase if needed
   (  class_id(DB_Object_V_Id, db_object_v),
      same_or_descendant(DB_Object_V_Id, _, Class_Id)
   -> Object = Object0 % already has a db_object_v ancestor
   ;  obj_rebase((object_v -> db_object_v), Object0, Object) ),

   % Check key if any
   get_key(Class_Id, Key),
   (  Key \== []
   ->
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
      db_recordz(DB_Key, Object)
   ).

%
% Record all solutions of the Pred.
% It is like findall/3, but use the last arg of Pred
% as a template.
% Use the same Options as db_put_objects.
% When Options = [] will raise exception on key duplicates.
%

db_put_objects(DB_Key, Pred, Options) :-

   call(Pred, Object),
   db_put_object(DB_Key, Object, Options),
   fail ; true.

%
% Leave only matched objects from DB by a search criteria
% Do not unify unbounded fields in DB (which are matched).
%
% filter_on_db(+DB_Key, ?Field_Names, ?Field_Values) :-
%
/*
filter_on_db(_, [], []) :- !.

filter_on_db(DB_Key, Field_Names, Field_Values) :-

  ground(DB_Key),

  findall(Obj_Ref,
          named_args_unify(DB_Key, _, Field_Names, Field_Values,
                           w(Obj_Ref, _)),
          Found_Obj_Ref_List),

  findall(Obj_Ref, db_recorded(DB_Key, _, Obj_Ref), All_Obj_Ref_List),

  subtract(All_Obj_Ref_List, Found_Obj_Ref_List, To_Delete_List),
  maplist(db_erase, To_Delete_List).
*/
% Copy from DB_In to DB_Out all filtered by Pred
/*
db_search(DB_In, DB_Out, Pred) :-

  db_recorded(DB_In, Term),
  once(call(Pred, Term)),
  db_recordz(DB_Out, Term),
  fail
  ;
  true.

db_copy(DB_In, DB_Out) :-

  db_recorded(DB_In, Term),
  db_recordz(DB_Out, Term),
  fail
  ;
  true.
*/
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
% db_to_list(+DB_Key, ?Functor, ?List)
%
/*
db_to_list(DB_Key, Functor, List) :-

  ground(DB_Key),
  findall(Object,
          (db_recorded(DB_Key, Object), functor(Object, Functor, _)),
          List).
*/
%
% On bt return all records selected by Query
%
/*
db_iterate(DB_Key, Query, Object) :-

   db_iterate(DB_Key, Query, true, Object).


db_iterate(DB_Key, Query, Filter_Pred, w(DB_Ref, Pred)) :-

   ground(Query),

   db_object_class(DB_Key, Class),
   resolve_args(Class, Query, Arg_Query),
   spec_term(Class, Spec_Term),
   functor(Spec_Term, _, Arity),
   functor(Pred, Class, Arity),
   db_recorded(DB_Key, Pred, DB_Ref),
   check_record(Arg_Query, Pred),
   ( Filter_Pred = true
   -> true
   ; once(call(Filter_Pred, Pred))
   ).

db_iterate_replace2(DB_Key, Pred, Query, Filter_Pred) :-

   db_iterate(DB_Key, Query, Filter_Pred, w(DB_Ref, Obj_In)),
   once(call(Pred, Obj_In, Obj_Out, _)),
   db_erase(DB_Ref),
   db_put_object(DB_Key, Obj_Out, [overwrite]),
   fail
   ;
   true.

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
       (   db_iterate(DB_Key, Query, w(DB_Ref, Obj_In)),
           once(call(Pred, Obj_In, Obj_Out, Is_Succ)),
           db_erase(DB_Ref),
           db_put_object(DB_Key, Obj_Out, [overwrite]),
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
*/

db_move_all_data(From_DB, To_DB) :-

  db_recorded(From_DB, Record, From_Ref),
  db_recordz(To_DB, Record),
  db_erase(From_Ref),
  fail
  ;
  true.

% Get a number of recrods

db_size(DB_Key, N) :-

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



