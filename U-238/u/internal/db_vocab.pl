% Maintain the list of open DBs

:- module(db_vocab,
	  [db_vocab_clear/1,    % ?DB_Key
	   db_vocab_local_db/3,
	   db_vocab_local_db_add/3
	  ]).

:- use_module(u(internal/db_i), [prolog:message/3]).

% Optimized for JIT-indexing

% db_ids(DB_Key, Local_Class_Id, DB_Class_Id)
:- dynamic db_ids/3.


db_vocab_clear(DB_Key) :-

   retractall(db_ids(DB_Key, _, _)).

db_vocab_local_db(DB_Key, Local_Class_Id, DB_Class_Id) :-

   db_ids(DB_Key, Local_Class_Id, DB_Class_Id), !.

db_vocab_local_db_add(DB_Key, Local_Class_Id, DB_Class_Id) :-

   (   db_ids(DB_Key, Local_Class_Id, DB_Class_Id)
   ->  throw(error(db_system_bad_state(
	  'db_vocab_local_db_add: db_ids(~w, ~w, ~w) already exists',
	  [DB_Key, Local_Class_Id, DB_Class_Id])))
   ;   assertz(db_ids(DB_Key, Local_Class_Id, DB_Class_Id))
   ).
