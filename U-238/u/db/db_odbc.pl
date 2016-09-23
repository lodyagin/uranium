:- module(db_odbc,
          [
	   map_db_type/2, % ?Type, ?DB_Type
	   nulls_to_unbounds_fields/2,
	   nulls_to_unbounds_obj/2,
	   odbc_connection/3,
	   odbc_erase/3,     % +ODBC_Db, +Obj, +Ctx
	   odbc_table_column_ext/8,
           odbc_recorded/3,  % +ODBC_Db, +Obj, +Ctx
           table_class/3,    % +ODBC_Db, +Table, -Class
           table_class/4,    % +ODBC_Db, +Schema, +Table, -Class
           table_column_ext/4,
           update_password/3 % +DSN, +User, +Password
          ]).

:- use_module(library(odbc)).
:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(ur_lists)).
:- use_module(u(util/lambda)).

% The predicate to store passwords.
:- dynamic dsn_user_password/3.
% The predicate to store connections.
:- dynamic dsn_user_connection/3.

%
% library(odbc) extension
%

%% odbc_table_column_ext(+Connection, -Schema, ?Table, ?Column, -Type, -Length, -Precision).
% Extends odbc:odbc_table_column to views also
odbc_table_column_ext(Connection, Schema, Table, Column, Type, Length, Precision, Scale) :-
    table_column_ext(Connection, Table, Column, Row),
    odbc:column_facet(table_owner(Schema), Row),
    odbc:column_facet(type_name(Type), Row),
    odbc:column_facet(length(Length), Row),
    odbc:column_facet(precision(Precision), Row),
    ignore(odbc:column_facet(scale(Scale), Row)).

% Extends odbc:table_column to views also
table_column_ext(Connection, Table, Column, Tuple) :-
	(   var(Table)
	->  odbc_current_table(Connection, Table, type(TV)),
	    ( TV == 'TABLE' ; TV == 'VIEW' ) 
	;   true
	),
	(   ground(Column)		% force determinism
	->  odbc:odbc_column(Connection, Table, Tuple),
	    arg(4, Tuple, Column), !
	;   odbc:odbc_column(Connection, Table, Tuple),
	    arg(4, Tuple, Column)
	).

%
%
%

%% odbc_db_key(DB, DSN, User) is semidet.
odbc_db_key(odbc(DSN, User), DSN, User) :- !.
odbc_db_key(odbc(DSN, User, Password), DSN, User) :-
   update_password(DSN, User,Password).

odbc_connection(ODBC_Db, Connection, Ctx) :-
   odbc_db_key(ODBC_Db, DSN, User),
   dsn_user_connection(DSN, User, Connection, Ctx).

%% odbc_erase(+ODBC_Db, +Obj, +Ctx) is det.
%
% Deletes those objects from ODBC_DB 
% that can be unified with Obj.
%
odbc_erase(ODBC_Db, Obj, Ctx) :-
   must_be(nonvar, Obj),
   odbc_db_key(ODBC_Db, DSN, User),
   dsn_user_connection(DSN, User, Connection, Ctx),
   sql_delete(Obj, SQL_C),
   string_codes(SQL, SQL_C),
   odbc_query(Connection, SQL).
   

%% odbc_recorded(+ODBC_Db, +Obj, +Ctx) is nondet.
%
% Unify Obj with database records. If Obj is nonvar it must be
% of some class returned by table_class/3.
%
odbc_recorded(ODBC_Db, Obj, Ctx) :-
  must_be(nonvar, Obj),
  odbc_db_key(ODBC_Db, DSN, User),
  dsn_user_connection(DSN, User, Connection, Ctx),
  obj_sql(Obj, Fields, SQL),
  findall_fields(Obj, true, false, VFields),
  findall(DBType, ( member(v(_, _, Type), VFields), map_db_type(Type, DBType) ), Types),
  odbc_query(Connection, SQL, Row, [types(Types), null(_)]),
  Row =.. [_|Values],
  obj_unify(Obj, 
            [db_key|Fields], 
            [odbc(DSN, User)|Values]).

map_db_type(varchar2, string) :- !.
map_db_type(char, string) :- !.
%map_db_type(float, double) :- !. Not in packages-odbc V7.2.3
map_db_type(rational, string) :- !.
map_db_type(date, string) :- !.
map_db_type(X, X).

obj_sql(Obj, Fields, SQL) :-
  functor(Obj, Class, _),
  class_fields_new(Class, Fields), % select these
%  findall(Fld-Val, (findall_fields(Obj, true, false, L), member(Fld-Val, L)), Where),
%  table_class_name(Table, Class),
  sql_select(Obj, Fields, SQLC),
  string_codes(SQL, SQLC).

%% sql_delete(+Obj, -SQL) is det.
%
% Constructs SQL for delete all objects 
% that are unified with Obj.
%
sql_delete(Obj, SQL) :-
  functor(Obj, Class, _),
  table_class_name(Table, Class),
  findall_fields(\_^V^_^ground(V), Obj, true, _, Fields),
  phrase(sql(delete, Table, Fields), SQL).

sql_select(Obj, Fields, SQL) :-
  functor(Obj, Class, _),
  table_class_name(Table, Class),
  findall_fields(\_^V^_^ground(V), Obj, true, _, Where),
  phrase(sql(select, Table, Fields, Where), SQL).

sql(delete, Table, Fields) --> 
  { atom_codes(Table, TableC) },
  "delete from ", TableC, " where ",
  sql_where_list(Fields).

sql(select, Table, Fields, Where) -->
  { atom_codes(Table, TableC) },
  "select ", sql_fields_for_select(Fields), " from ", TableC,
  sql_where_part(Where).

sql_where_part([]) --> !.
sql_where_part(Where) -->
  " where ", sql_where_list(Where).

sql_fields_for_select([F]) --> !,
  { atom_codes(F, FC) },
  FC.    
sql_fields_for_select([F0, F1|T]) -->
  { atom_codes(F0, F0C) },
  F0C, ",",
  sql_fields_for_select([F1|T]).
  
sql_where_list([]) --> [].
sql_where_list([V|T]) -->
  sql_where_condition(V),
  sql_where_list_cont(T).

sql_where_list_cont([]) --> [].
sql_where_list_cont([V|T]) --> 
  " and ", sql_where_condition(V),
  sql_where_list_cont(T).

sql_where_condition(v(Name, Value, _)) -->
  { Value=='$null$', !, atom_codes(Name, NameC) },
  NameC, " is null ".
sql_where_condition(v(Name, Value, _)) -->
  { atom_codes(Name, NameC) },
  NameC, "=",
  sql_value(Value).

sql_value(null(_)) -->
  "NULL".
sql_value(Value) -->
  { atom(Value), !,
    format(codes(Str), "'~a'", Value) },
  Str.
sql_value(Value) -->
  { string(Value), !,
    format(codes(Str), "'~s'", Value) },
  Str.
sql_value(Value) -->
  { integer(Value), !,
    atom_codes(Value, Int) },
  Int.
sql_value(Value) -->
    { float(Value), !,
      format(codes(Float), "~f", [Value])
    },
    Float.
sql_value(Value) -->
  { throw(error(domain_error(sql_type, Value), _)) }.

%% table_class(+ODBC_Db, +Table, -Class) is det.
%
% Creates or return existing Class which corresponds to Table in ODBC_Db
%
table_class(ODBC_Db, Table, Class) :-
   Ctx = context(table_class/3),
   odbc_db_key(ODBC_Db, _, User), 
   table_class_int(ODBC_Db, User, Table, Class, Ctx).

%% table_class(+ODBC_Db, +Schema, +Table, -Class) is det.
%
% Creates or return existing Class which corresponds to Schema.Table in ODBC_Db
%
table_class(ODBC_Db, Schema, Table, Class) :-
   Ctx = context(table_class/3),
   must_be(nonvar, Schema), 
   table_class_int(ODBC_Db, Schema, Table, Class, Ctx).

table_class_int(ODBC_Db, Schema, Table, Class, Ctx) :-
   table_class_name(Table, Class),
   (  class_name(Class)
   -> true % the Class already exists
   ;  table_class_create(ODBC_Db, Schema, Table, Class, odbc_table_v, Ctx)
   ).

table_class_name(Table, Class) :-
   nonvar(Table), !,
   downcase_atom(Table, TableL),
   atom_concat(TableL, '_v', Class).
table_class_name(TableU, Class) :-
   atom_concat(TableL, '_v', Class),
   upcase_atom(TableL, TableU).

table_class_create(ODBC_Db, Schema, Table, Class, Parent, Ctx) :-
   odbc_connection(ODBC_Db, Connection, Ctx),
   upcase_atom(Table, TableU),
   upcase_atom(Schema, SchemaU),
   findall(C:T,
           (
	       odbc_table_column_ext(Connection, SchemaU, TableU, C0, T0, L, Prec, Scale),
	       downcase_atom(C0, C),
	       (  oracle_table_v:data_type(T0, T0, L, Prec, Scale, T1) 
	       -> functor(T1, T, _)
	       ;  T = error
	       )
	   ),
           FieldsTypes),
   (  FieldsTypes == [] -> throw(error(db_no_such_table(ODBC_Db, TableU), Ctx))
    ; true
    ),
   (
       odbc_table_primary_key(Connection, TableU, Key0)
   ->
       (  Key0 == [_|_] -> Key = Key0 ; Key = [Key0] ),
       maplist(downcase_atom, Key, KeyL),
       class_create(Class, Parent, FieldsTypes, KeyL)
   ;
       class_create(Class, Parent, FieldsTypes)
   ).	       

%% update_password(+DSN, +User, +Password) is det.
%
% Updates a password for DSN/User
%
update_password(DSN, User, Password) :-
   must_be(atom, DSN),
   must_be(atom, User),
   must_be(atom, Password),
   retractall(dsn_user_password(DSN, User, _)),
   assertz(dsn_user_password(DSN, User, Password)).

dsn_user_connection(DSN, User, Connection, Ctx) :-
   (  dsn_user_connection(DSN, User, Connection) -> true
   ;  (  dsn_user_password(DSN, User, Password) -> true
      ;  throw(error(no_password(DSN, User), Ctx))
      ),
      odbc_connect(DSN, Connection,
                   [user(User), password(Password), open(once)
                    %,null(_)
		   ]),
      assertz(dsn_user_connection(DSN, User, Connection))
   ).

nulls_to_unbounds_obj(Obj0, Obj) :-
    Obj0/class ^= Class,
    class_fields_new(Class, Fields),
    length(Fields, N),
    length(Values, N),
    obj_rewrite(Obj0, Fields, Values0, Values, Obj),
    nulls_to_unbounds_fields(Values0, Values).

nulls_to_unbounds_fields(Values0, Values) :-
    findall(V, ( member(V0, Values0), (V0 == '$null$' -> true; V0=V) ), Values).
