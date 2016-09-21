:- module(plsql_fun_v,
	  [
	      execute_fun/2
%	      fun_block//1
	  ]).

:- use_module(library(error)).
:- use_module(library(odbc)).
:- use_module(u(v)).
:- use_module(u(db/db_odbc)).
:- use_module(v(plsql_proc_v)).

new_class(plsql_fun_v, plsql_proc_v, []).

'plsql_fun_v?'(Obj, '#return_type', ReturnType) :-
    obj_field(Obj, '#procname', Proc), nonvar(Proc),
    obj_field(Obj, '#pkg', Package), nonvar(Package),
    obj_field(Obj, '#db', DB), nonvar(DB),
    table_class(DB, sys, all_arguments, AAClass),
    upcase_atom(Proc, ProcU),
    upcase_atom(Package, PackageU),
    obj_construct(AAClass,
		  [object_name, package_name, data_level, argument_name,
		   data_length, data_precision, data_type, pls_type],
		  [ProcU, PackageU, 0.0, '$null$',
		  Length, Precision, DataType, PlsType], AA),
    odbc_recorded(DB, AA, _), !,
    oracle_table_v:data_type(DataType, PlsType, Length, Precision, ReturnType).


execute_fun(Obj, Result) :-
    Obj/['#out_args', '#inout_args'] ^= [[],[]], !, % no out pars
    Obj/['#conn', '#return_type'] ^= [Conn, Type],
    phrase(fun_block(Type, Obj), LC),
    string_codes(LS, LC),
    debug(sql, "~s", [LS]),
    odbc_query(Conn, LS),
    phrase(fun_block_result(Type), LC1),
    string_codes(LS1, LC1),
    debug(sql, "~s", [LS1]),
    odbc_query(Conn, LS1, row(Result0)), !,
    convert_result(Type, Result0, Result).

convert_result(integer, Result0, Result) :- !,
    atom_number(Result0, Result).
convert_result(varchar2(_), Result, Result) :- !.

fun_block(ResultType, Obj) -->
    "declare\n", "res ", result_type(ResultType), ";\n\t", plsql_proc_v:declare_pars(Obj), !,
    "\nbegin\n\t",
    plsql_proc_v:assignments(Obj), "\n\t",
    "res := ", plsql_proc_v:proc_call(Obj), ";\n\t",
    "delete from ", result_table(ResultType), ";\n\t",
    "insert into ", result_table(ResultType), " values(res);\n",
    "end;\n".
fun_block(ResultType, Obj) -->
    "declare\n", "res ", result_type(ResultType), ";",
    "\nbegin\n\t",
    plsql_proc_v:assignments(Obj), "\n\t",
    "res := ", plsql_proc_v:proc_call(Obj), ";\n\t",
    "delete from ", result_table(ResultType), ";\n\t",
    "insert into ", result_table(ResultType), " values(res);\n",
    "end;\n".

fun_block_result(ResultType) -->
    "select result from ", result_table(ResultType).

result_table(RT) -->
    "slod_test_", result_type_table_subname(RT), "_result".

result_type(integer) -->
    !, "integer".
result_type(varchar2(R)) -->
    { N is round(R), atom_codes(N, NC) },
    "varchar2(", NC, ")".

result_type_table_subname(varchar2(_)) --> "varchar2".
result_type_table_subname(integer) --> "integer".
