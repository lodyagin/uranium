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
'odbc_table_v?'(Obj, '?max', Value) :-
    obj_unify(Obj, ['+field', '+db'], [Field, DB]),
    (  ( var(Field) ; var(DB) ) -> true
     ;
       Ctx = context('odbc_table_v?/3', _),
       named_arg(Obj, Field, _, Type),
       map_db_type(Type, DBType),
       obj_field(Obj, '#table', Table),
       format(string(SQL), "select max(~a) from ~a", [Field, Table]),
       odbc_connection(DB, Conn, Ctx),
       odbc_query(Conn, SQL, row(Value), [types([DBType])])
    ).
       
