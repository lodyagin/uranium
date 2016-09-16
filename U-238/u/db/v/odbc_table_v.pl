:- module(odbc_table_v,
          [
          ]).

new_class(odbc_table_v, db_object_v, []).

'odbc_table_v?'(Obj, schema, Schema) :-
    obj_field(Obj, db_key, ODBC),
    ( nonvar(ODBC), functor(ODBC, odbc, _) -> arg(2, ODBC, Schema) ; true).
