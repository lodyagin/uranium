:- module(plsql_proc_v,
	  [
	      execute_proc/2,
	      par_by_mode/4, %+Obj, ?Mode, -Fld, -PLSQ_Type
	      proc_block//1,
	      proc_call//1,
	      select//1
	  ]).

:- use_module(library(error)).
:- use_module(library(odbc)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(u(v)).
:- use_module(u(db/db_odbc)).
:- use_module(u(internal/objects_i)).

:- dynamic object_module:plsql_argument/3.

new_class(plsql_proc_v, object_v,
	  ['#conn'
	  ]).

'plsql_proc_v?'(Obj, '#db', DB) :-
    obj_field(Obj, '#conn', Conn),
    odbc_connection(DB, Conn, context('plsql_proc_v?'/3)).
	
'plsql_proc_v?'(Obj, '#procname', Proc) :-
    obj_field(Obj, class, Class),
    atomic_list_concat([W0, W1, v], '_', Class),
    ( [W0, W1] == [plsql, proc] -> true;
      Proc = W1).

'plsql_proc_v?'(Obj, '#pkg', Pkg) :-
    obj_field(Obj, class, Class),
    atomic_list_concat([W0, W1, v], '_', Class),
    ( [W0, W1] == [plsql, proc] -> true;
      Pkg = W0).

'plsql_proc_v?'(Obj, '#out_args', Args) :-
    findall(F-T, par_by_mode(Obj, out, F, T), Args).

'plsql_proc_v?'(Obj, '#inout_args', Args) :-
    findall(F-T, par_by_mode(Obj, inout, F, T), Args).

'plsql_proc_v?'(Obj, '#select_fun', Name) :-
    obj_field(Obj, '#procname', PN),
    atomic_list_concat([odbc, PN], '_', Name).
    
'plsql_proc_v?'(Obj, '#select_results', Flds) :-
    obj_field(Obj, '#out_args', Outs),
    obj_field(Obj, '#inout_args', InOuts),
    append(Outs, InOuts, Flds).
    
execute_proc(Obj0, Obj) :-
    Obj0/['#out_args', '#inout_args'] ^= [[],[]], !, % no out pars
    obj_field(Obj0, '#conn', Conn),
    phrase(proc_block(Obj0), LC),
    string_codes(LS, LC),
    debug(sql, "~s", [LS]),
    odbc_query(Conn, LS),
    Obj = Obj0.
execute_proc(Obj0, Obj) :-
    obj_field(Obj0, '#conn', Conn),
    phrase(select(Obj0), LC),
    string_codes(LS, LC),
    odbc_query(Conn, LS, Res),
    row_obj(Obj0, Res, Obj).

row_obj(Obj0, Row, Obj) :-
    Row =.. [row|Vs],
    obj_field(Obj0, '#select_results', Flds),
    pairs_keys_values(Flds, Names, _),
    obj_rewrite(Obj0, Names, _, Vs, Obj).

par_by_mode(Obj, Mode, Fld, PLSQL_Type) :-
    findall_fields(Obj, true, false, Flds),
    member(v(Fld, _, Type), Flds),
    nonvar(Type),
    object_module:plsql_argument(Type, PLSQL_Type, Mode).

select(Obj) -->
    { Obj/'#select_fun' ^= Fun,
      atom_codes(Fun, FunC)
    },
    "select ", FunC, "(", select_pars(Obj), ") from dual".

select_pars(Obj) -->
    { findall(Name-Value,
	      ( findall_fields(Obj, true, false, Vs),
		member(v(Name, Value, _), Vs),
		nonvar(Value)
	      ), FldVals)
    },
    proc_params2(FldVals).

proc_block(Obj) -->
    "declare\n", declare_pars(Obj), !, "begin\n", assignments(Obj), "\n\t", proc_call(Obj), ";\nend;".
proc_block(Obj) -->
    "begin\n", assignments(Obj), "\n\t", proc_call(Obj), ";\nend;".

declare_pars(Obj) -->
    {   setof(Var-Type,
	      Par^Value^Type0^OutArgs^InOutArgs^( (
		   Obj/'#out_args'^=OutArgs, member(Par-Type, OutArgs)
		 ; Obj/'#inout_args'^=InOutArgs, member(Par-Type, InOutArgs)
		 ; used_field(Obj, Par, Value, Type0),
		   u_object(Value),
		   Type^=Value/'#type'
		),
	        atom_concat(v, Par, Var)
	      ), Vars
	    ) -> true ; Vars = []
    },
    declare_pars2(Vars).

used_field(Obj, Name, Value, Type) :-
    findall_fields(Obj, true, false, Vs),
    member(v(Name, Value, Type), Vs),
    nonvar(Value).

declare_pars2([]) --> !.
declare_pars2([Var-Type|T]) -->
    declare_par(Var, Type),
    declare_pars2(T).

declare_par(Var, Type) -->
    { atom_codes(Var, VarC), atom_codes(Type, TypeC) },
    "\t", VarC, " ", TypeC, ";\n".

findall_objval_fields(Obj, FldVals) :-
    findall(Name-Value,
	    (   used_field(Obj, Name, Value),
		u_object(Value)
	    ), FldVals).

proc_call(Obj) -->
    { Obj/['#procname', '#pkg'] ^= [ProcNameA, PkgA],
      nonvar(ProcNameA),
      atom_codes(ProcNameA, ProcName),
      atom_codes(PkgA, Pkg)
    },
    Pkg, ".", ProcName, "(", proc_params(Obj), ")".

proc_params(Obj) -->
    { findall(Name-Value,
	      ( findall_fields(Obj, true, false, Vs),
		member(v(Name, Value, _), Vs),
		(  nonvar(Value) -> true
		 ; ( par_by_mode(Obj, out, Name, _) ; par_by_mode(Obj, inout, Name, _) ),
		   atom_concat(v, Name, Value0),
		   Value=v(Value0)
		)
	      ), FldVals)
    },
    proc_params2(FldVals).

proc_params2([]) --> "".
proc_params2([Fld-v(Var)]) --> !,
    { atom_codes(Fld, FldC),
      atom_codes(Var, VarC)
    },
    FldC, "=>", VarC.
proc_params2([Fld-Val]) --> 
    { u_object(Val), !,
      atom_codes(Fld, FldC),
      var_name(Fld, VarC)
    },
    FldC, "=>", VarC.
proc_params2([Fld-Val]) --> !,
    { atom_codes(Fld, FldC) },
    FldC, "=>", right_side(Val).
proc_params2([Fld-Val|T]) -->
    proc_params2([Fld-Val]),
    ",", proc_params2(T).

assignments(Obj) -->
    { findall([Var]-Value,
	      ( used_field(Obj, Par, Value, _), u_object(Value),
		atom_concat(v, Par, Var)
	      ),
	      LeftRights)
    },
    assignments2(LeftRights).

assignments2([]) --> !.
assignments2([Lefts-Obj|T]) -->
    { u_object(Obj), !,
      findall([Field|Lefts]-Value,
	      used_field(Obj, Field, Value, _),
	      FieldVals)
    },
    assignments2(FieldVals),
    assignments2(T).
assignments2([Lefts-Right|T]) -->
    left_side(Lefts), ":=", right_side(Right), ";\n",
    assignments2(T).

left_side([A]) --> !,
    { atom_codes(A, AC) }, "\t", AC.
left_side([A, B]) --> !,
    { atom_codes(A, AC), atom_codes(B, BC) },
    "\t", BC, ".", AC.
left_side([A|T]) -->
    { atom_codes(A, AC) }, 
    left_side(T), ".", AC.

right_side(false) --> !, "FALSE".
right_side(true) --> !, "TRUE".
right_side('$null$') --> !, "NULL".
right_side(V) --> { var(V), !} , "NULL".
right_side(A) -->
    { number(A), !, atom_codes(A, AC) },
    AC.
right_side(A) -->
    { atom(A), !, atom_codes(A, AC) },
    "'", AC, "'".
right_side(timestamp(Y,Mo,D,H,Mi,S,_)) --> !,
    { format_time(codes(SC), "%F %T", date(Y, Mo, D, H, Mi, S, 0, -, false))
    },
    "TO_TIMESTAMP('", SC, "', 'YYYY-MM-DD HH24:MI:SS')".
right_side(A) -->
    { atom_codes(A, AC) }, AC.

var_name(Fld, [VC|FldC]) :-
    atom_char(v, VC),
    atom_codes(Fld, FldC).

typedef(boolean_in, [plsql - arg(boolean, in)]).
typedef(number_in,  [plsql - arg(number, in)]).
typedef(number_out, [plsql - arg(number, out)]).
typedef(varchar2_in, [plsql - arg(varchar2, in)]).
