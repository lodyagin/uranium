%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(db_pg,
          [pg_create_table/2,  % +Connection, +Object

           pg_erase/1,
           pg_recordz/2,
           pg_recorded/3,
           pg_object_class/2
           ]).

:- use_module(u(db/postgres/postgres)).
:- use_module(u(ur_lists)).
:- use_module(u(v)).
:- use_module(u(logging)).

:- dynamic pl_pg_type/3,  % Prolog_Type, Postgres_Type, Conv_Pred
           postgres_db/1,  % Connection
           postgres_inited/0,
           postgres_host/1,
           postgres_user/1.


pg_recorded(pg(Connect), Object, pg(Connect, Table, Oid)) :-

    get_connected(Connect), !,

    (  compound(Object)
    -> object_row(Object, Term),
       term_row(Term, Row),
       functor(Term, Table, _)
    ;  dpg_table(Connect, Table), % for each table in db
       postgres:pgTABLE(Connect, Table, Column_List),
       %class_ensure_created(Table),
       length(Column_List, Arity),
       Arity1 is Arity + 1,
       functor(Row, Table, Arity1)
    ),
    pg_select(Connect, Row),
    row_term(Row, Term2, Oid),
    row_object(Term2, Object).


pg_erase(pg(Connect, Table, Oid)) :-

    get_connected(Connect), !,
    format(atom(Sql),
           'delete from ~a where oid=~d',
           [Table, Oid]),
    postgres:pgSQL(Connect, Sql, _), !.
    

pg_recordz(pg(Connect), Object) :-

    get_connected(Connect, create),
    functor(Object, Class, Arity),
    Table = Class,
    
    (  postgres:pgTABLE(Connect, Table, Column_List)
    -> length(Column_List, N),
       (  N \= Arity
       -> write_log(['Object/postgres table mismatch, object:',
                    Object, 'table:', Column_List],
                    [lf(1, before), lf(1)]),
          fail
       ;  true
       )
    ;  pg_create_table(Connect, Object)
    ),

    object_row(Object, Row),
    write_log(['pg_insert:', Row], [logger(postgres), lf(1)]),
    pg_begin(Connect),
    pg_insert(Connect, Row),
    pg_commit(Connect).


pg_object_class(pg(Connect), Class) :-

    get_connected(Connect), !,
    dpg_table(Connect, Class),
    atom_concat(_, '_v', Class).
        

row_term(Row, Term, Oid) :-
        
   Row =.. [Functor|Row_Args],
   append(Args, [Oid], Row_Args), !,
   maplist(null_free_var, Args, Args2),
   Term =.. [Functor| Args2].

term_row(Term, Row) :-
        
   Term =.. [Functor| Args],
   append(Args, [_], Row_Args), !, 
   Row =.. [Functor|Row_Args].

object_row(Object, Row) :-

   Object =.. [Class|Object_Args],
   class_fields(Class, Fields),
   maplist(field_pl_pg(Class), Fields, Object_Args, PG_Args),
   Row =.. [Class|PG_Args].
   
row_object(Row, Object) :-

   Row =.. [Class|PG_Args],
   class_fields(Class, Fields),
   maplist(field_pl_pg(Class), Fields, Object_Args, PG_Args),
   Object =.. [Class|Object_Args].
   

null_free_var(null(_), _) :- !.

null_free_var(A, A).

get_connected(Connection) :-

    get_connected(Connection, no).

get_connected(Connection, _) :-

    postgres_db(Connection), !. % already connected

get_connected(Connection, Create) :-

    get_inited,
    
    (   connect_db(Connection)
    ->  assert(postgres_db(Connection))
    ;   Create = create
    ->  create_db(Connection),
	get_connected(Connection, no)
    ;   fail
    ).

connect_db(Connection) :-

    (postgres_host(Host) -> true ; Host = localhost),
    (postgres_user(User) -> true ; User = prolog),
    
    format(atom(String), 'host=~a user=~a dbname=~a',
           [Host, User, Connection]),
    
    pg_connect(String, Connection).

create_db(Connection) :-

    format(atom(Sql),
           'create database ~a',
           [Connection]),
    postgres:pgSQL(prolog, Sql, _).

get_inited :-

    postgres_inited -> true ;
    pg_init,
    connect_db(prolog),
    assert(postgres_inited).


pg_obj_table(Object, Create_Table_Spec) :-

   with_output_to(atom(Create_Table_Spec),
                  pg_obj_table_write(Object)).

pg_create_table(Connection, Object) :-

   pg_obj_table(Object, Sql),
   write_log(Sql, [logger(postgres), lf(1)]),
   postgres:pgSQL(Connection, Sql, _).

pg_obj_table_write(Object) :-
        
   functor(Object, Class, _),
   class_fields(Class, [First_Field|Fields_Tail]),
   Table_Name = Class,
   format('create table ~a (', Table_Name),
   field_write(Class, first, First_Field),
   maplist(field_write(Class, no), Fields_Tail),
   key_write(Class),
   write(') with oids').

key_write(Class) :-

   get_key(Class, Key),
   (  Key \= []
   -> write(', unique('),
      write_delimited(write, ', ', Key),
      write(')')
   ;  true
   ).

      
field_write(Class, First, Field) :-

   class_field_type(Class, Field, PL_Type),
   (  var(PL_Type)
   -> PG_Type = varchar
   ;  pl_pg_type(PL_Type, PG_Type, _)
   -> true
   ;  PL_Type = PG_Type
   ),
        
   (First \= first -> write(', ') ; true),

   format('~a ~a', [Field, PG_Type]).


field_pl_pg(Class, Field, PL_Val, PG_Val) :-

   var(PL_Val), var(PG_Val) ->
   true

   ;
        
   class_field_type(Class, Field, PL_Type),
   (  var(PL_Type)
   -> PG_Val = PL_Val
   ;  pl_pg_type(PL_Type, _, Conv_Pred)
   -> (   call(Conv_Pred, PL_Val, PG_Val)
      ->  true
      ;   write_log(['Unable to convert', PL_Val, '<->', PG_Val,
	             'by', Conv_Pred], [lf(1)]),
          fail
      )
   ;  PG_Val = PL_Val
   ).

                   
dpg_table(Connection, Table) :-

   postgres:pgSQL(Connection,
             'select c.relname FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace WHERE c.relkind IN (\'r\',\'\') AND n.nspname NOT IN (\'pg_catalog\', \'pg_toast\') AND pg_catalog.pg_table_is_visible(c.oid)', Res), !,
   member([Table], Res).
   
