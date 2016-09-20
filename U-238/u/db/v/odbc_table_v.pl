:- module(odbc_table_v,
          [
          ]).

:- use_module(u(db/db_odbc)).
:- use_module(u(v)).

new_class(odbc_table_v, db_object_v, ['+db',
	   '+field']).

'odbc_table_v?'(Obj, '#schema', Schema) :-
    obj_field(Obj, db_key, ODBC),
    ( nonvar(ODBC), functor(ODBC, odbc, _) -> arg(2, ODBC, Schema) ; true).

'odbc_table_v?'(Obj, '#table', TableName) :-
    obj_unify(Obj, [db_key, class], [ODBC, Class]),
    table_class(ODBC, TableName, Class). %NB it can create new class based on the class field
                                         % (consider usage of table-descendants)

% Max field value
'odbc_table_v?'(Obj, '?max', Value) :- sql_field_query(Obj, '?max', Value).

% Min field value
'odbc_table_v?'(Obj, '?min', Value) :- sql_field_query(Obj, '?min', Value).

sql_field_query(Obj, SQLField, Value) :-
    field_sql(SQLField, SQLExpr), !,
    obj_unify(Obj, ['+field', '+db'], [Field, DB]),
    (  ( var(Field) ; var(DB) ) -> true
     ;
       Ctx = context('odbc_table_v?/3', _),
       named_arg(Obj, Field, _, Type),
       map_db_type(Type, DBType),
       obj_field(Obj, '#table', Table),
       format(string(Fmt), "select ~s from ~~a", [SQLExpr]),
       format(string(SQL), Fmt, [Field, Table]),
       odbc_connection(DB, Conn, Ctx),
       odbc_query(Conn, SQL, row(Value), [types([DBType])])
    ).
       
field_sql('?max', "max(~a)").
field_sql('?min', "min(~a)").
