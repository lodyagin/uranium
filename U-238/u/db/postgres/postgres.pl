% File:      postgres.pl
% Author(s): Hakun Skardhamar
% Contact:   hakun.skardhamar@mail.dk
%
% Copyright (C) Hakun Skardhamar, 2001
%
% This program is free software; you can redistribute it and/or modify it under the
% terms of the GNU Library General Public License as published by the Free Software
% Foundation; either version 2 of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.
%
% You should have received a copy of the GNU Library General Public License along
% with oracle.pl; if not, write to the Free Software Foundation, Inc.,
% 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
%


:-module(postgres,[	
                        op(700,xfx,in),
			op(700,xfx,like),
			op(700,xfx,~),
			op(990,xfx,:=),
 			op(990,fx,:=),

                        pg_init/0,
			pg_declare_knowledge_base/2,
			pg_revoke_knowledge_base/2,
			pg_connect/2,
			pg_disconnect/1,
			pg_begin/1,
			pg_commit/1,
			pg_rollback/1,
			pg_new_cursor_name/1,
			pg_close_cursor/1,
			pg_close_all_cursors/1,
			pg_import_with_cursor/4,
			pg_import_with_cursor/5,
			pg_import/4,
			pg_insert/2,
			pg_update/2,
			pg_select_with_cursor/2,
			pg_select_with_cursor/3,
			pg_select_with_cursor/4,
			pg_select/1,
			pg_select/2,
			pg_consult/1,
			pg_descriptions/1,
			pg_store_from_file/2,
			pg_create_logic_index/3,
			pg_drop_logic_index/1,
			pg_select_indexed/1,
			pg_select_indexed/2,
			pg_store/2]).

:- use_module(u(logging)).

%:-module_transparent pg_init/0.

/* ---------------------------------------------------------------------------- */
/* OPERATORS ------------------------------------------------------------------ */
/* ---------------------------------------------------------------------------- */
pg_init:-
	load_foreign_library(postgres),
	flag(pg_import_count,_,0),
	flag(pg_import_max,_,200),
	flag(dwim_correct_goal,_,off).

/* ---------------------------------------------------------------------------- */
/* CONNECTIONS ---------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_connect(_,Con):- nonvar(Con),
	' PG GetState'(connection,Con,_),!,fail.
pg_connect(Db,Con):- nonvar(Db),nonvar(Con),
	pgCONNECT(Db,Con),
	pgTYPES(Con,L),
	' PG SetState'(connection,Con,open),
	' PG SetState'(types,Con,L).

pg_disconnect(Con):- nonvar(Con),
	' PG GetState'(connection,Con,_),
	pg_close_all_cursors(Con),
	' PG DelState'(_,Con,_),
	pgDISCONNECT(Con),!.	

/* ---------------------------------------------------------------------------- */
/* TRANSACTIONS --------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_begin(Con):- nonvar(Con),
	' PG GetState'(transaction,Con,_),!,fail.
pg_begin(Con):- nonvar(Con),
	pgSQL(Con,'BEGIN',_),
	' PG SetState'(transaction,Con,open).

pg_commit(Con):- nonvar(Con),
	pg_close_all_cursors(Con),
	' PG DelState'(transaction,Con,_),
	pgSQL(Con,'COMMIT',_).

pg_rollback(Con):- nonvar(Con),
	pg_close_all_cursors(Con),
	' PG DelState'(transaction,Con,_),
	pgSQL(Con,'ROLLBACK',_).

pg_new_cursor_name(C):- var(C),
	flag(pg_cursor,N,N+1),
	atom_concat(cur,N,C).

pg_close_cursor(Cur):- nonvar(Cur),
	' PG GetState'(cursor,Con,Cur),
	' PG CloseCursor'(Con,Cur).

pg_close_all_cursors(Con):- nonvar(Con),
	' PG GetState'(cursor,Con,Cur),
	' PG CloseCursor'(Con,Cur),
        fail.
pg_close_all_cursors(_).
	
/* ---------------------------------------------------------------------------- */
/* Knowledgebase management --------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_declare_knowledge_base(Con,KB):- ' PG GetState'(knowledge_base,Con,KB),!.
pg_declare_knowledge_base(Con,KB):- nonvar(Con),nonvar(KB),
	' PG SetState'(knowledge_base,Con,KB),	
	ClH = exception(undefined_predicate,C,A),	
	ClB = (pg_consult(C)->A=retry;A=error),	
        !,
        (clause(ClH,ClB)-> true ; asserta(user:(ClH:-ClB))).

pg_revoke_knowledge_base(Con,KB):- nonvar(Con),nonvar(KB),
	' PG DelState'(knowledge_base,Con,KB),	
	Cl = (exception(undefined_predicate,C,A):-(pg_consult(C)->A=retry;A=error)),	
        (' PG GetState'(knowledge_base,_,_) -> true ; retract(user:Cl)).

/* ---------------------------------------------------------------------------- */
/* IMPORT TABLE --------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_import_with_cursor(Con,T/A,FormatLst,SQL):-
	pg_import_with_cursor(Con,T/A,FormatLst,SQL,100).
pg_import_with_cursor(Con,T/A,FormatLst,SQL,Bsize):-
	' PG GetState'(connection,Con,_),
	' PG CrTypelist'(FormatLst,Typelist),
	' PG ImportTableFormat'(T/A,[],Head,Alst),
	user:abolish(T/A),
	assertz(user:(Head:-
		postgres:' PG GetState'(transaction,Con,_),
		postgres:' PG ImportQueryWithCursor'(Con,SQL,Alst,Typelist,Bsize) )),
	!.
	
' PG CrTypelist'([H|T],[_/H|R]):-
	' PG CrTypelist'(T,R).
' PG CrTypelist'([],[]).

/* Used by pg_import */
' PG ImportQueryWithCursor'(Con,SQL,Alst,Typelist,Bsize):-
	' PG QueryWithCursor'(Con,C,SQL,_),
	(	' PG ImportTableWithCursor'(Con,C,start,Alst,Typelist,Bsize);
		' PG CloseCursor'(Con,C),fail
	).

' PG ImportQuery'(Con,SQL,Alst,Typelist):-
	pgSQL(Con,SQL,Solutions),
	postgres:' PG ImportTable'(Solutions,Alst,Typelist).

/* Used by pg_import */
' PG ImportTableFormat'(T/0,X,H,X):- !,	H =.. [T|X].
' PG ImportTableFormat'(T/A,Y,H,X):-
	A1 is A - 1,
	' PG ImportTableFormat'(T/A1,[_|Y],H,X).

/* Used by pg_import */
' PG QueryWithCursor'(Con,C,SQL,R):-
	pg_new_cursor_name(C),
	concat_atom(['DECLARE ', C,' CURSOR FOR ',SQL],'',S),
	pgSQL(Con,S,R),
	' PG SetState'(cursor,Con,C).

pg_import(Con,T/A,FormatLst,SQL):-
	' PG GetState'(connection,Con,_),
	' PG CrTypelist'(FormatLst,Typelist),
	' PG ImportTableFormat'(T/A,[],Head,Alst),
	user:abolish(T/A),
	assertz(user:(Head:- postgres:' PG ImportQuery'(Con,SQL,Alst,Typelist) )),
	!.
	
/* ---------------------------------------------------------------------------- */
/* INSERT INTO TABLE ---------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_insert(Con,T):-
	' PG GetState'(transaction,Con,_),
	' PG ParseInsert'(Con,T,SQL),
        write_log(pgSQL(Con, SQL, _), 
	          [logger(postgres), quoted(if_needed), lf(1)]),
        (  atom(SQL) -> true
        ; write_log('SQL is not an atom!', [lf(1)])
        ),
	pgSQL(Con,SQL,_),!.

' PG ParseInsert'(Con,T,S):-
	T =.. [Tname|Args],
	' PG TableColons'(Con,Tname,Tlst),
	' PG InsertFormat'(Tlst,Args,NewArgs),
	concat_atom(NewArgs,',',Values),
	write_log([' PG ParseInsert Values'| Values], 
	          [logger(postgres), quoted(if_needed), lf(1)]),
	concat_atom(['insert into ',Tname,' values(',Values,')'],'',S).
		
/* Used by pg_insert */
' PG InsertFormat'([_/T],[V],[V1]):- !,
        write_log(' PG FormatToString'(T,V,V1), 
	          [logger(postgres), quoted(if_needed), lf(1)]),
	' PG FormatToString'(T,V,V1),
        write_log(' PG FormatToString'(T,V,V1), 
	          [logger(postgres), quoted(if_needed), lf(1)]).
' PG InsertFormat'([_/Type|R],[V|Q],[V1|Q1]):-
        write_log(' PG FormatToString'(Type,V,V1), 
	          [logger(postgres), quoted(if_needed), lf(1)]),
	' PG FormatToString'(Type,V,V1),
        write_log(' PG FormatToString'(Type,V,V1), 
	          [logger(postgres), quoted(if_needed), lf(1)]),
	' PG InsertFormat'(R,Q,Q1).
	
/* ---------------------------------------------------------------------------- */
/* UPDATE TABLE --------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_update(Con,TabAction):-
	' PG GetState'(transaction,Con,_),
	' PG ParseUpdate'(TabAction,Con,Update),
	pgSQL(Con,Update,_),!.

/* Used by pg_update      */	
' PG ParseUpdate'(Clause,Con,Update):-
	Clause =.. [Tab|Action1],
	' PG TableColons'(Con,Tab,Typelist),
	' PG GetOrder'(Action1,Typelist,_,Action2),
	' PG GetGroupBy'(Action2,Typelist,_,Action3),
	' PG InsertNames'(Action3,Typelist,Action4),
	' PG GetCondUpdate'(Action4,Typelist,Set,SetWhere),
	' PG FormatWhere'(SetWhere,Where,Typelist),
	' PG FormatSet'(Set,Typelist,Set1),
	concat_atom(['update ',Tab,Set1,Where],'',Update).

/* Used by ' PG ParseUpdate' */
' PG GetCondUpdate'([_^X|T],Types,ST,SWT):- nonvar(X),X = '',!,
	' PG GetCondUpdate'(T,Types,ST,SWT).
' PG GetCondUpdate'([_^X|T],Types,ST,SWT):- var(X),!,
	' PG GetCondUpdate'(T,Types,ST,SWT).
' PG GetCondUpdate'([N^(:=(V,C))|T],Types,[N=V|ST],[C1|SWT]):- !,
	' PG CompleatCond'(N,C,C1),
	' PG GetCondUpdate'(T,Types,ST,SWT).
' PG GetCondUpdate'([N^(:=(C))|T],Types,ST,[C1|SWT]):- !,
	' PG CompleatCond'(N,C,C1),
	' PG GetCondUpdate'(T,Types,ST,SWT).
' PG GetCondUpdate'([N^V|T],Types,[N=V|ST],SWT):- !,
	' PG GetCondUpdate'(T,Types,ST,SWT).
' PG GetCondUpdate'([],_,[],[]).

/* Used by ' PG ParseUpdate' */
' PG FormatSet'([H|L],T,S):-
	' PG FormatSet1'([H|L],T,S1),
	concat_atom([' set '|S1],'',S).

/* Used by ' PG FormatSet' */
' PG FormatSet1'([M=null(_)],_,[M,'=',null]):- !.
' PG FormatSet1'([M=null(_),T|R],L,[M,'=',null,', '|S]):- !,
	' PG FormatSet1'([T|R],L,S).
' PG FormatSet1'([M=V],L,[M,'=',V1]):- !,
	memberchk(M/T,L),
	' PG FormatToString'(T,V,V1).
' PG FormatSet1'([M=V,X|R],L,[M,'=',V1,', '|S]):- !,
	memberchk(M/T,L),
	' PG FormatToString'(T,V,V1),
	' PG FormatSet1'([X|R],L,S).

/* ---------------------------------------------------------------------------- */
/* SELECT TABLE --------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_select_with_cursor(Con,Clause):-
	pg_select_with_cursor(Con,Clause,_,100).
pg_select_with_cursor(Con,Clause,Cur):-
	pg_select_with_cursor(Con,Clause,Cur,100).
pg_select_with_cursor(Con,Clause,Cur,Bsize):-
	' PG GetState'(transaction,Con,_),
	' PG ParseSelect'(Clause,Con,Arglst,Typelist,Select),
	pg_new_cursor_name(Cur),
	concat_atom(['DECLARE ',Cur,' CURSOR FOR ',Select],'',Select1),
	' PG SetState'(cursor,Con,Cur),
	pgSQL(Con,Select1,_),
	!,
	(	' PG ImportTableWithCursor'(Con,Cur,start,Arglst,Typelist,Bsize);
		' PG CloseCursor'(Con,Cur),fail
	).

pg_select(Clause):-
	' PG GetState'(connection,Con,_),
	pg_select(Con,Clause).

pg_select(Con,Clause):-
	' PG GetState'(connection,Con,_),
	' PG ParseSelect'(Clause,Con,Arglst,Typelist,Select),
/*	write(Select),nl,   */
	pgSQL(Con,Select,Solutions),
	!,
	' PG ImportTable'(Solutions,Arglst,Typelist).
        	
/* ---ParseSelect-------------------------------------------------------------- */
/* Used by pg_select_with_cursor, pg_select      */	
' PG ParseSelect'(Clause,Con,Arguments,Typelist2,Select):-
	Clause =.. [Tab|Action1],
	' PG TableColons'(Con,Tab,Typelist0),
	append(Typelist0,[oid/oid],Typelist),
	' PG InsertNames'(Action1,Typelist,Action2),
	' PG GetOrder'(Action2,Typelist,OrderSt,Action3),
	' PG GetGroupBy'(Action3,Typelist,GroupBySt,Action4),
	' PG GetCondSelect'(Action4,Typelist,Where,Arguments,SelArgsL,Typelist1),
	(SelArgsL = [] ->
		Typelist = [N/T|_],SelArgsL1 = [N], Typelist2 = [N/T];
		SelArgsL1 = SelArgsL, Typelist2 = Typelist1
	),
	concat_atom(SelArgsL1,',',SelArgs),
	' PG FormatWhere'(Where,Where1,Typelist),
	concat_atom(['select ',SelArgs,' from ',Tab,Where1,GroupBySt,OrderSt],'',Select).

/* Used by ' PG ParseSelect' */
' PG GetCondSelect'([N^X|T],Types,W,[X|R],[N|NR],[N/Type|TR]):- var(X),!,
	memberchk(N/Type,Types),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([_^''|T],Types,W,R,NR,TR):- !,
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^(:=(V,C))|T],Types,[C1|W],R,NR,TR):- ground(V),V='',!,
	' PG CompleatCond'(N,C,C1),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^(:=(V,C))|T],Types,[(N = V),C1|W],R,NR,TR):- ground(V),!,
	' PG CompleatCond'(N,C,C1),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^(:=(V1,C))|T],Types,[(~(N,V1M)),C1|W],VR,[Select|NR],Tlst):-
	V1 \= null(_),
	V1 \= (_ << _),
	\+ (V1 = [VH|_],nonvar(VH),VH = (_ << _)),!,
	memberchk(N/Type,Types),
	' PG MatchPatern'(V1,V1M),
	' PG CompleatValue'(V1,N,Type,Types,Select,V,TL),
	append(V,R,VR),
	append(TL,TR,Tlst),
	' PG CompleatCond'(N,C,C1),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^(:=(V1,C))|T],Types,[C1|W],VR,[Select|NR],Tlst):- !,
	memberchk(N/Type,Types),
	' PG CompleatValue'(V1,N,Type,Types,Select,V,TL),
	append(V,R,VR),
	append(TL,TR,Tlst),
	' PG CompleatCond'(N,C,C1),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^(:=(C))|T],Types,[C1|W],R,NR,TR):- !,
	' PG CompleatCond'(N,C,C1),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^V|T],Types,[(N = V)|W],R,NR,TR):-
	ground(V),!,
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^V1|T],Types,[(~(N,V1M))|W],VR,[Select|NR],Tlst):-
	V1 \= null(_),
	V1 \= (_ << _),
	\+ (V1 = [VH|_],nonvar(VH),VH = (_ << _)),!,
	memberchk(N/Type,Types),
	' PG MatchPatern'(V1,V1M),
	' PG CompleatValue'(V1,N,Type,Types,Select,V,TL),
	append(V,R,VR),
	append(TL,TR,Tlst),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([N^V1|T],Types,W,VR,[Select|NR],Tlst):- !,
	memberchk(N/Type,Types),
	' PG CompleatValue'(V1,N,Type,Types,Select,V,TL),
	append(V,R,VR),
	append(TL,TR,Tlst),
	' PG GetCondSelect'(T,Types,W,R,NR,TR).
' PG GetCondSelect'([],_,[],[],[],[]).

/* Used by ' PG GetCondSelect' */
' PG MatchPatern'(T,S):-
	term_to_atom(T,A),
	atom_concat(A,'.',A1),
	atom_to_term(A1,T1,_),
	free_variables(T1, L),
	' PG MaskVars1'(L),
	term_to_atom(T1,S1),
	' PG MaskSpace'(S1,S2),
	concat_atom(['^',S2,'$'],'',S).

/* Used by ' PG MatchPatern' */
' PG MaskVars1'([]).
' PG MaskVars1'([H|T]):- H = '.*', ' PG MaskVars1'(T).

/* Used by ' PG MatchPatern' */
' PG MaskSpace'(S1,S2):- name(S1,L),' PG MaskSpace1'(L,L2),name(S2,L2).

/* Used by ' PG MaskSpace' */
' PG MaskSpace1'([124|T],[46,42|R]):- 	!,' PG RemoveTail'(T,T1,0),' PG MaskSpace1'(T1,R).
' PG MaskSpace1'([40|T], [92,40|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([41|T], [92,41|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([91|T], [92,91|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([92|T], [92,92|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([93|T], [92,93|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([123|T],[92,123|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([125|T],[92,125|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([32|T], [32,42|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([X|T],[X|R]):- 	!,' PG MaskSpace1'(T,R).
' PG MaskSpace1'([],[]).

/* Used by ' PG MaskSpace1' */
' PG RemoveTail'([93|T],[93|T],0):- !.
' PG RemoveTail'([91|T],T1,N):- N > 0,!,N1 is N+1, ' PG RemoveTail'(T,T1,N1).
' PG RemoveTail'([93|T],T1,N):- N > 0,!,N1 is N-1, ' PG RemoveTail'(T,T1,N1).
' PG RemoveTail'([_|T],T1,N):- ' PG RemoveTail'(T,T1,N).

/* Used by ' PG GetCondSelect' */
' PG CompleatValue'(V,N,T,_,N,[V],[N/T]):- var(V),!.
' PG CompleatValue'(null(_),_,_,_,_,_,_):- !,fail.
' PG CompleatValue'((V<<AGG),N1,T1,Types,AGG1,[V],[N/F]):-
	AGG =.. [A,N],nonvar(A),!,
	(var(N) -> N = N1,T = T1; memberchk(N/T,Types)),
	' PG AggFormat'(T,A,F),
	concat_atom([A,'(',N,')'],'',AGG1).
' PG CompleatValue'([(V<<AGG)|T],N,Type,Types,AGG1,VL,TL):- nonvar(AGG),!,
	' PG AggList'([(V<<AGG)|T],N/Type,Types,AGGL,VL,TL),
	concat_atom(AGGL,',',AGG1).
' PG CompleatValue'(V,N,T,_,N,[V],[N/T]).

/* Used by ' PG CompleatValue', ' PG AggList' */
' PG AggFormat'(_,count,int4):- !.
' PG AggFormat'(interval,_,interval):- !.
' PG AggFormat'(X,_,X):-' PG NumberFormat'(X).

/* Used by ' PG CompleatValue' */
' PG AggList'([(V<<AGG)|AT],N1/T1,Types,[AGG1|AR],[V|VR],[N/F|TR]):-
	AGG =.. [A,N],nonvar(A),!,
	(var(N) -> N = N1,T = T1; memberchk(N/T,Types)),
	' PG AggFormat'(T,A,F),
	concat_atom([A,'(',N,')'],'',AGG1),
	' PG AggList'(AT,N1/T,Types,AR,VR,TR).
' PG AggList'([],_,_,[],[],[]).

/* ---ImportTable-------------------------------------------------------------- */
/* Used by pg_select */
' PG ImportTable'([X],X1,Tlist):- !,' PG Convert'(X,Tlist,X1).
' PG ImportTable'([X|_],X1,Tlist):- ' PG Convert'(X,Tlist,X1).
' PG ImportTable'([_|T],X,TL):- !,' PG ImportTable'(T,X,TL).

/* ---------------------------------------------------------------------------- */
/* Consult Table -------------------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_consult(Pred/Arrity):-
	once((	' PG GetState'(connection,Con,_),
		' PG GetState'(knowledge_base,Con,TAB),
		' PG DefinitionExist'(Con,TAB,Pred/Arrity)  	)),
	once(' PG CollectOldPredicates'),
	' PG Consult'(Pred/Arrity,Con,TAB).
pg_consult(Pred/Arrity):- current_predicate(Pred/Arrity),
	get_time(Time),
	' PG SetState'(predicate_import_time,Time,Pred/Arrity),
	flag(pg_import_count,N,N+1),!.
pg_consult(Action):- Action \= (_/_),
	once((	' PG GetState'(connection,Con,_),
		' PG GetState'(knowledge_base,Con,TAB),
		' PG DefinitionExist'(Con,TAB,Action)  	)),
	' PG ConsultAction'(Action,Con,TAB).

' PG DefinitionExist'(Con,TAB,Item):-
	Check =.. [TAB,(:=(=(Item)))>>1,X<<count(_)],
	pg_select(Con,Check),
	X > 0.

/* Used by pg_consult */
' PG Consult'(Pred/Arrity,_,TAB):-
	Select =.. [TAB,Pred/Arrity,:=(>(0))/\1,Clause],
	pg_select(_,Select),
	' PG Consult1'(Clause).
	
' PG Consult1'(Clause):-
	Clause = (:- Action),!,
	call(user:Action),!,fail.
' PG Consult1'(Clause):-
	assertz(user:Clause),
	fail.

' PG ConsultAction'(Action,_,TAB):-
	Select =.. [TAB,Action,''/\1,Clause],
	pg_select(_,Select),
	Clause = (:- A),
	call(user:A),fail.
' PG ConsultAction'(_,_,_).

' PG CollectOldPredicates':-
	flag(pg_import_max,Max,Max),
	flag(pg_import_count,N,N),
	N > Max,
	setof(T=P,pg_state(predicate_import_time,T,P),L),
	length(L,Len),
	X is round(Len - Max*0.75),
	' PG StackChk'(OnStack),
	' PG DeleteOldPredicates'(X,L,OnStack).
' PG CollectOldPredicates'.
	
' PG DeleteOldPredicates'(X,[_ = P|T],S):- X > 0,!,
	(member(P,S) -> true;
		abolish(user:P),
		' PG DelState'(predicate_import_time,_,P),
		flag(pg_import_count,N,N-1)
	),
	X1 is X-1,
	' PG DeleteOldPredicates'(X1,T,S).
' PG DeleteOldPredicates'(_,T,_):-
	length(T,L),
	flag(pg_import_count,_,L).
		
/* ---------------------------------------------------------------------------- */
/* Predicate description ------------------------------------------------------ */
/* ---------------------------------------------------------------------------- */

pg_descriptions(KB):-
	' PG GetState'(knowledge_base,Con,KB),
	' PG GetState'(connection,Con,_),
	Command =.. [KB,(P/A)/\1,0,D],
	pg_select(Con,Command),
	concat_atom([P,/,A,':\t',D],'',S),
	write(S),nl,
	fail.
pg_descriptions(_).



/* ---------------------------------------------------------------------------- */
/* Save predicate in table ---------------------------------------------------- */
/* ---------------------------------------------------------------------------- */
pg_store(P/A,Base):-
	' PG PrepareStore'(P/A,Base,H,Con),
	' PG Store'(H,P/A,Con,Base),!.
	
' PG PrepareStore'(P/A,Base,H,Con):-
	current_predicate(P/A),	
	' PG GetState'(knowledge_base,Con,Base),
	' PG GetState'(connection,Con,_),
	' PG GetState'(transaction,Con,_),
	' PG TableColons'(Con,Base,[N/_,_,_]),
	concat_atom(['delete from ',Base,' where ',N,' = ''',P,/,A,''''],'',Del),
	pgSQL(Con,Del,_),
	' PG ArgLst'(0,A,L),
	H =.. [P|L],
	flag(pg_clausecount,_,1).

/* Used by pg_store */
' PG ArgLst'(X,X,[]):- !.
' PG ArgLst'(X,Y,[_|T]):- X1 is X + 1,' PG ArgLst'(X1,Y,T).
	
/* Used by pg_store */
' PG Store'(H,P,C,B):-
	clause(H,Body),
	( Body == true -> Clause = H ; Clause = (H:-Body)),
	flag(pg_clausecount,N,N+1),
	V =.. [B,P,N,Clause],
	pg_insert(C,V),
	fail.
' PG Store'(_,_,_,_).

pg_store_from_file(File,KB):-
	' PG GetState'(knowledge_base,Con,KB),
	' PG GetState'(connection,Con,open),
	' PG GetState'(transaction,Con,open),
	see(File),
	read(T),
	' PG PredicateList'(T,[],L),
	seen,
	consult(File),
	forall(member(P,L),(pg_store(P,KB),abolish(P))).
	
' PG PredicateList'(end_of_file,L,L):- !.
' PG PredicateList'(T,L,List):-
	' PG DoTerm'(T,L,L1),
	read(T1),
	' PG PredicateList'(T1,L1,List).
			
' PG DoTerm'((:- _),L,L):- !.
' PG DoTerm'((H:- _),L,L1):- !,
	H =.. [P|Args],
	length(Args,A),
	(memberchk(P/A,L)-> L1 = L; L1 = [P/A|L]).
' PG DoTerm'(H,L,L1):-
	H =.. [P|Args],
	length(Args,A),
	(memberchk(P/A,L)->  L1 = L; L1 = [P/A|L]).
' PG DoTerm'(_,L,L).

	
/* ---------------------------------------------------------------------------- */
/* Things for select and update ----------------------------------------------- */
/* ---------------------------------------------------------------------------- */
/* Used by ' PG ParseSelect', ' PG ParseUpdate' */
' PG InsertNames'([C|T],[Name/_|R],[Name^C|Q]):- 	
	var(C),!,' PG InsertNames'(T,R,Q).
' PG InsertNames'([C|T],[H|T1],[C|Q]):-
	' PG InsertNamesGetName'(C,Name),	
	!,delete([H|T1],Name/_,R),' PG InsertNames'(T,R,Q).
' PG InsertNames'([C|T],[Name/_|R],[NC|Q]):-
	' PG InsertNamesInCore'(C,Name,NC),!,	
	' PG InsertNames'(T,R,Q).
' PG InsertNames'([C|T],[Name/_|R],[Name^C|Q]):- 	
	' PG InsertNames'(T,R,Q).
' PG InsertNames'([],_,[]):- !.

' PG InsertNamesInCore'(C\/X,Name,NC\/X):- nonvar(X),!,
	' PG InsertNamesInCore'(C,Name,NC).
' PG InsertNamesInCore'(C/\X,Name,NC/\X):- nonvar(X),!,
	' PG InsertNamesInCore'(C,Name,NC).
' PG InsertNamesInCore'(C>>X,Name,NC>>X):- nonvar(X),!,
	' PG InsertNamesInCore'(C,Name,NC).
' PG InsertNamesInCore'(C,Name,Name^C1):- nonvar(C),C=Name^C1,!.
' PG InsertNamesInCore'(C,Name,Name^C).

' PG InsertNamesGetName'(C \/ X,Name):- nonvar(C),nonvar(X),!,
	' PG InsertNamesGetName'(C,Name).
' PG InsertNamesGetName'(C /\ X,Name):- nonvar(C),nonvar(X),!,
	' PG InsertNamesGetName'(C,Name).
' PG InsertNamesGetName'(C >> X,Name):- nonvar(C),nonvar(X),!,
	' PG InsertNamesGetName'(C,Name).
' PG InsertNamesGetName'(Name^_,Name):- atom(Name).

/* Used by ' PG GetCondSelect', ' PG GetCondUpdate' */
' PG CompleatCond'(N,(C1,C2),(C11,C21)):- !,
	' PG CompleatCond'(N,C1,C11),
	' PG CompleatCond'(N,C2,C21).
' PG CompleatCond'(N,(C1;C2),(C11;C21)):- !,
	' PG CompleatCond'(N,C1,C11),
	' PG CompleatCond'(N,C2,C21).
' PG CompleatCond'(N,null(_),=(N,null(_))):- !.
' PG CompleatCond'(N,\+(C),not(C1)):- !,
	' PG CompleatCond'(N,C,C1).
' PG CompleatCond'(N,C,C1):-
	C =.. [Op,CC],!,
	C1 =.. [Op,N,CC].
' PG CompleatCond'(_,C,C):-
	C =.. [_,_,_],!.
' PG CompleatCond'(N,C,C1):-
	C1 =.. ['=',N,C].

/* Used by ' PG ParseSelect', ' PG ParseUpdate' */
' PG FormatWhere'([],'',_).
' PG FormatWhere'(L,S,TL):-
	' PG FormatWhere1'(L,Lst,TL),
	concat_atom([' where '|Lst],' ',S).

/* Used by ' PG FormatWhere' */
' PG FormatWhere1'([],[],_).
' PG FormatWhere1'([A],C,TL):- !,
	' PG FormatWhere1'(A,A1,TL),
	append(['('|A1],[')'],C).
' PG FormatWhere1'([A,Y|T],C,TL):- !,
	' PG FormatWhere1'(A,A1,TL),
	' PG FormatWhere1'([Y|T],R,TL),
	append(['('|A1],[') and '|R],C).
' PG FormatWhere1'((A,B),D,TL):- !,
	' PG FormatWhere1'(A,A1,TL),
	' PG FormatWhere1'(B,B1,TL),
	append(['('|A1],[' and '|B1],C),
	append(C,[')'],D).
' PG FormatWhere1'((A;B),D,TL):- !,
	' PG FormatWhere1'(A,A1,TL),
	' PG FormatWhere1'(B,B1,TL),
	append(['('|A1],[' or '|B1],C),
	append(C,[')'],D).
' PG FormatWhere1'(not(C),[' not '|C1],TL):- !,
	' PG FormatWhere1'(C,C1,TL).
' PG FormatWhere1'((A = null(_)),[A,' is ',null],_):- !.
' PG FormatWhere1'(in(A,B),[A,' in (',B2,')'],TL):-
	memberchk(A/T,TL),
	' PG FormatInList'(T,B,B1),
	concat_atom(B1,',',B2).
' PG FormatWhere1'(C,[A,Op1,B1],TL):-
	C =.. [Op,A,B],
	memberchk(A/T,TL),
	' PG Operator'(Op,Op1),
	' PG FormatToString'(T,B,B1).

/* Used by ' PG FormatWhere1' */
' PG Operator'(',',' and '):-!.
' PG Operator'(';',' or '):-!.		
' PG Operator'('=<','<='):-!.
' PG Operator'('\\=','<>'):-!.
' PG Operator'(X,X).

/* Used by ' PG FormatWhere1' */
' PG FormatInList'(Type,[B|T],[B1|R]):-
	' PG FormatToString'(Type,B,B1),
	' PG FormatInList'(Type,T,R).
' PG FormatInList'(_,[],[]).
	
/* Used by ' PG ParseSelect', ' PG ParseUpdate' */
' PG GetOrder'(A,L,OLSt,B):-
	' PG GetOrder1'(A,L,OL,B),
	sort(OL,OLS),
	' PG OrderStatement'(OLS,OLSt).

/* Used by ' PG GetOrder' */
' PG GetOrder1'([AP|B],[N/_|L],[P/N/' asc'|OL],[A|T]):- nonvar(AP),AP = A/\P,!,
	' PG GetOrder1'(B,L,OL,T).
' PG GetOrder1'([AP|B],[N/_|L],[P/N/' desc'|OL],[A|T]):- nonvar(AP),AP = A\/P,!,
	' PG GetOrder1'(B,L,OL,T).
' PG GetOrder1'([A|B],[_|L],OL,[A|T]):- !,
	' PG GetOrder1'(B,L,OL,T).
' PG GetOrder1'([],_,[],[]).
	
/* Used by ' PG GetOrder' */
' PG OrderStatement'([X|Y],S):-
	' PG OrderSt'([X|Y],SL),
	concat_atom(SL,',',S1),
	atom_concat(' order by ',S1,S).
' PG OrderStatement'([],'').
	
/* Used by ' PG OrderStatement', ' PG GroupBySt' */
' PG OrderSt'([_/N/T|Y],[NT|L]):-
	atom_concat(N,T,NT),
	' PG OrderSt'(Y,L).
' PG OrderSt'([],[]).
	
/* Used by ' PG ParseSelect', ' PG ParseUpdate' */
' PG GetGroupBy'(A,L,OLSt,B):-
	' PG GetGroupBy1'(A,L,OL,B),
	sort(OL,OLS),
	' PG GroupByStatement'(OLS,OLSt).
	
/* Used by ' PG GetGroupBy' */
' PG GetGroupBy1'([AP|B],[N/_|L],[P/N|OL],[A|T]):- nonvar(AP),AP = A>>P,!,
	' PG GetGroupBy1'(B,L,OL,T).
' PG GetGroupBy1'([A|B],[_|L],OL,[A|T]):- !,
	' PG GetGroupBy1'(B,L,OL,T).
' PG GetGroupBy1'([],_,[],[]).
	
/* Used by ' PG GetGroupBy' */
' PG GroupByStatement'([X|Y],S):-
	' PG GroupBySt'([X|Y],SL),
	concat_atom(SL,',',S1),
	atom_concat(' group by ',S1,S).
' PG GroupByStatement'([],'').
	
/* Used by ' PG GroupByStatement' */
' PG GroupBySt'([_/N|Y],[N|L]):- ' PG OrderSt'(Y,L).
' PG GroupBySt'([],[]).

/* ---------------------------------------------------------------------------- */
/* MISCELANEOUS HELP FUNCTIONS ------------------------------------------------ */
/* ---------------------------------------------------------------------------- */
/* Used by pg_import, pg_insert, pg_store, ' PG ParseSelect', ' PG ParseUpdate' */
' PG TableColons'(Con,Tab,Typelist):-
	' PG GetState'(tablelayout,Con,Tab/Typelist),!.
' PG TableColons'(Con,Tab,Typelist):-
	pgTABLE(Con,Tab,T1),
	' PG GetState'(connection,Con,_),
	' PG GetState'(types,Con,L),
	' PG TableColons1'(T1,Typelist,L),
	' PG SetState'(tablelayout,Con,Tab/Typelist).

/* Used by ' PG TableColons'  */	
' PG TableColons1'([],[],_).
' PG TableColons1'([N/ID|T],[N/F|R],Types):-
	memberchk(F/ID,Types),	
	' PG TableColons1'(T,R,Types).

/* Used by pg_import, pg_select_with_cursor */
' PG ImportTableWithCursor'(_,_,[X|_],X1,Tlist,_):-
	' PG Convert'(X,Tlist,X1).
' PG ImportTableWithCursor'(Con,C,[_|T],X,TL,B):- !,
	' PG ImportTableWithCursor'(Con,C,T,X,TL,B).
' PG ImportTableWithCursor'(Con,C,_,X,TL,B):-
	concat_atom(['FETCH FORWARD ',B,' IN ',C],'',S),
	pgSQL(Con,S,Q),
	Q = [_|_],
	' PG ImportTableWithCursor'(Con,C,Q,X,TL,B).

/* Used by ' PG InsertFormat', ' PG FormatSet1', ' PG FormatWhere1', ' PG FormatInList' */
' PG FormatToString'(_,null(_),null):- !.

' PG FormatToString'(bytea,V,S):-
	 ' PG EscQuoteToDbQ'(V,S2),
	 concat_atom(['E', '''',S2,''''],'',S), !.

' PG FormatToString'(F,V,S):- ' PG NumberFormat'(F),!,term_to_atom(V,S).
' PG FormatToString'(bool,V,S):- ' PG FormatToBool'(V,S1),!,concat_atom(['''',S1,''''],'',S).
' PG FormatToString'(point,point(A,B),S):- !,sformat(S,'''(~f,~f)''',[A,B]).
' PG FormatToString'(circle,circle(point(A,B),C),S):- !,sformat(S,'''<(~f,~f),~f>''',[A,B,C]).
' PG FormatToString'(lseg,line(point(A,B),point(C,D)),S):- !,sformat(S,'''[(~f,~f),(~f,~f)]''',[A,B,C,D]).
' PG FormatToString'(box,box(point(A,B),point(C,D)),S):- !,sformat(S,'''(~f,~f),(~f,~f)''',[A,B,C,D]).
' PG FormatToString'(path,path(closed,L),S):- !,
	' PG FormatPath'(L,Body),
	concat_atom(Body,',',B),
	concat_atom(['''(',B,')'''],'',S).
' PG FormatToString'(path,path(open,L),S):- !,
	' PG FormatPath'(L,Body),
	concat_atom(Body,',',B),
	concat_atom(['''[',B,']'''],'',S).
' PG FormatToString'(polygon,polygon(L),S):- !,
	' PG FormatPath'(L,Body),
	concat_atom(Body,',',B),
	concat_atom(['''(',B,')'''],'',S).
' PG FormatToString'(_,V,S):-
	' PG NameVars'(V,V1),
	 (atom(V1) -> S1 = V1; term_to_atom(V1,S1)),
	 ' PG EscQuoteToDbQ'(S1,S2),
	 concat_atom(['''',S2,''''],'',S), !.

' PG FormatToBool'(true,t).
' PG FormatToBool'(fail,f).
' PG FormatToBool'(yes,t).
' PG FormatToBool'(no,f).
' PG FormatToBool'(1,t).
' PG FormatToBool'(0,f).
' PG FormatToBool'(t,t).
' PG FormatToBool'(f,f).

/* Used by ' PG FormatToString' */
' PG NameVars'(T,T1):- copy_term(T,T1),numbervars(T1,'$VAR',0,_).

/* Used by ' PG FormatToString' */
' PG EscQuoteToDbQ'(S1,S2):-
	name(S1,L),
	' PG EscQuoteToDbQ1'(L,LL),
	name(S2,LL), !.
	
/* Used by ' PG EscQuoteToDbQ' */
' PG EscQuoteToDbQ1'([],[]) :- !.
' PG EscQuoteToDbQ1'([39|T],[39,39|TT]):- ' PG EscQuoteToDbQ1'(T,TT), !.
%' PG EscQuoteToDbQ1'([92|T],[92,92|TT]):- ' PG EscQuoteToDbQ1'(T,TT), !.
' PG EscQuoteToDbQ1'([X|T],[X|TT]):- ' PG EscQuoteToDbQ1'(T,TT), !.
	
/* Used by ' PG FormatToString' */
' PG FormatPath'([],[]).
' PG FormatPath'([point(A,B)|T],[AB|R]):-
	sformat(AB,'(~f,~f)',[A,B]),
	' PG FormatPath'(T,R).
	 	
/* Used by  */
' PG GeoSubstitute'(T,A,P):-
	name(A,[X|AL]),
	' PG GeoSubstitute1'([X|AL],BL),
	(T = box -> append([91|BL],[93,46],BLL); append(BL,[46],BLL) ),
	name(B,BLL),
	atom_to_term(B,B1,[]),
	' PG GeoToTerm'(T,X,B1,P).
	
' PG GeoToTerm'(point,_,[A,B],point(A,B)).
' PG GeoToTerm'(lseg,40,[[A,B],[C,D]],linesegment(point(A,B),point(C,D))).
' PG GeoToTerm'(lseg,91,[[A,B],[C,D]],line(point(A,B),point(C,D))).
' PG GeoToTerm'(line,91,[[A,B],[C,D]],line(point(A,B),point(C,D))).
' PG GeoToTerm'(box,_,[[A,B],[C,D]],box(point(A,B),point(C,D))).
' PG GeoToTerm'(circle,_,[[A,B],R],circle(point(A,B),R)).
' PG GeoToTerm'(path,40,L,path(closed,L1)):- ' PG ToPoints'(L,L1).
' PG GeoToTerm'(path,91,L,path(open,L1)):- ' PG ToPoints'(L,L1).
' PG GeoToTerm'(polygon,40,L,polygon(L1)):- ' PG ToPoints'(L,L1).

' PG ToPoints'([],[]).
' PG ToPoints'([[A,B]|R],[point(A,B)|T]):-
	' PG ToPoints'(R,T).
	
/* Used by  */
' PG AtomToTermIfPossible'(A,T):-
	catch(atom_to_term(A,T,_),error(syntax_error(_),string(_,_)),T = A).
	
/* Used by  */
' PG GeoSubstitute1'([40|T],[91|R]):- !,' PG GeoSubstitute1'(T,R).
' PG GeoSubstitute1'([41|T],[93|R]):- !,' PG GeoSubstitute1'(T,R).
' PG GeoSubstitute1'([60|T],[91|R]):- !,' PG GeoSubstitute1'(T,R).
' PG GeoSubstitute1'([62|T],[93|R]):- !,' PG GeoSubstitute1'(T,R).
' PG GeoSubstitute1'([X|T],[X|R]):- ' PG GeoSubstitute1'(T,R).
' PG GeoSubstitute1'([],[]).

/* Used by ' PG ImportTable', ' PG ImportTableWithCursor' */
' PG Convert'(_,_,[]):- !.
' PG Convert'([X|Y],[_/T|S],[X1|Y1]):-
	' PG ReadFromInput'(T,X,X1),
	' PG Convert'(Y,S,Y1).
' PG Convert'([],_,[]).

/* Used by ' PG Convert' */
' PG ReadFromInput'(_,null(_),null(_)):- !.
' PG ReadFromInput'(bool,T,P):- !,' PG FormatBool'(T,P).
' PG ReadFromInput'(F,X,P):- ' PG NumberFormat'(F),!,atom_to_term(X,P,_).
' PG ReadFromInput'(F,X,P):-
	memberchk(F,[point,lseg,path,box,circle,polygon,line]),!,
	' PG GeoSubstitute'(F,X,P).
' PG ReadFromInput'(_,X,X2):-
	' PG AtomToTermIfPossible'(X,X1),
	(var(X1) -> X2 = X; X2 = X1).

/* Used by ' PG AggFormat', ' PG FormatToString', ' PG ReadFromInput' */
' PG NumberFormat'(int2).
' PG NumberFormat'(int4).
' PG NumberFormat'(int8).
' PG NumberFormat'(float4).
' PG NumberFormat'(float8).
' PG NumberFormat'(numeric).
' PG NumberFormat'(oid).

' PG FormatBool'(t,true).
' PG FormatBool'(f,fail).

' PG CloseCursor'(Con,C):-
	atom_concat('CLOSE ', C, S),
	pgSQL(Con,S,_),
	' PG DelState'(cursor,Con,C).
		
/* ---------------------------------------------------------------------------- */
/* - Manage pg state ---------------------------------------------------------- */
/* Used by many */
' PG GetState'(Key,V1,V2):- current_predicate(pg_state/3), pg_state(Key,V1,V2).

' PG SetState'(Key,V1,V2):- current_predicate(pg_state/3), pg_state(Key,V1,V2), !,fail.
' PG SetState'(Key,V1,V2):- asserta(pg_state(Key,V1,V2)).

' PG DelState'(Key,V1,V2):- retractall(pg_state(Key,V1,V2)).
		
/* ---------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------- */
/* - Find predicates on the stack --------------------------------------------- */
' PG StackChk'(Lst):-
	prolog_current_frame(F),
	' PG StackChk1'(F,R),!,
	setof(X,member(X,R),Lst).
' PG StackChk'([]).
	
' PG StackChk1'(F,[]):- prolog_frame_attribute(F,top,true),!.
' PG StackChk1'(F,T):-
	' PG ChkFrame'(F,alternative,LA,A),
	' PG ChkFrame'(F,parent,LP,P),
	append(LA,LP,LAP),
	append(LAP,R,LAPR),
	append(LAPR,X,T),
	' PG StackChk2'(P,R),
	' PG StackChk2'(A,X).

' PG StackChk2'(none,[]):-!.
' PG StackChk2'(P,F):- ' PG StackChk1'(P,F).

' PG ChkFrame'(F,T,[Pr/Len],P):-
	prolog_frame_attribute(F,T,P),
	\+ prolog_frame_attribute(P,hidden,true),
	prolog_frame_attribute(P,clause,C),
	clause(H,_,C),
	H =.. [Pr|Args],
	length(Args,Len).
' PG ChkFrame'(_,_,[],none).

/* ---------------------------------------------------------------------------- */

pg_create_logic_index(Con,Tab,Col):-
	pg_drop_logic_index(Tab),
	pgTABLE(Con,Tab,TL),
	nth1(Col,TL,N/_),
	Term =.. [Tab,Con,N],
	asserta(Term),
	Call =.. [Tab,oid^Oid,N^Cond],
	pg_select(Con,Call),
	TabOid =.. [Tab,Oid],
	assertz(TabOid:-Cond),
	fail.
pg_create_logic_index(_,_,_).

pg_drop_logic_index(Tab):-
	abolish(Tab/2),
	abolish(Tab/1).

pg_select_indexed(Clause):-
	' PG GetState'(connection,Con,_),
	pg_select_indexed(Con,Clause).

pg_select_indexed(Con,Clause):-
	' PG GetState'(connection,Con,_),
	Clause =.. [Tab|Args],
	current_predicate(Tab/2),
	Chk =.. [Tab,Con,_],
	call(Chk),
	!,
	Taboid =.. [Tab,X],
	findall(X,Taboid,L),
	Call =.. [Tab,(oid^(:=(in(L))))|Args],
	pg_select(Con,Call).
