:- module(oracle_table_v,
	  [data_type/6]).

new_class(oracle_table_v, odbc_table_v, []).

%% data_type(DataType, PLSType, Length, Precision, Scale, PrologType
% NB DataType (the first arg) is not used (is some additional information there compared to PlsType?)
data_type(_, 'VARCHAR2', Length, _, _,     varchar2(Length)) :- !.
data_type(_, 'CHAR',     Length, _, _,     char(Length)) :- !.
data_type(_, 'INTEGER',  _,      _, _,     integer) :- !.
data_type(_, 'DECIMAL',  _,      _, 0,     integer) :- !.
data_type(_, 'NUMBER',   _,      _, Scale, float) :- var(Scale), !.
    


typedef(varchar2, []).

