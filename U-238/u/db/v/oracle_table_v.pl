:- module(oracle_table_v,
	  [data_type/5]).

new_class(oracle_table_v, odbc_table_v, []).

% NB DataType (the first arg) is not used (is some additional information there compared to PlsType?)
data_type(_, 'VARCHAR2', Length, _, varchar2(Length)) :- !.
data_type(_, 'INTEGER', _, _, integer) :- !.
    

typedef(varchar2, []).

