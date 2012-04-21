% -*- fill-column: 65; -*- 
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011, Sergei Lodyagin
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
% 
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(table_v_parse,
          [table_v_parse/2,
           table_v_cast/7
           ]).

/** <module> Parse tables
  */

:- use_module(u(ur_atoms)).
:- use_module(u(ur_terms)).
:- use_module(u(ur_lists)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(logging)).
:- use_module(u(internal/check_arg)).
:- use_module(library(xpath)).
:- use_module(library(lists)).

table_v_parse(Data, Object) :-

  get_table_header(Data, Header),
  get_table_rows(Data, Rows),
  obj_construct(table_v, [header, rows], [Header, Rows],
                Object).

get_table_header(DOM, Header) :-

    xpath(DOM, //table, TAB), !,
    xpath(TAB, //tr(1), TR), 
    !, % hm, really only first?

    findall(Cell,
            (xpath(TR, td(normalize_space), Cell);
             xpath(TR, th(normalize_space), Cell)),
            List
            ),
    maplist(normalize_name, List, Row_Names),
    Header =.. [table_header|Row_Names].

get_table_rows(DOM, Table_Data) :-

    xpath(DOM, //table, TAB), !, 
    findall(Row, 
            (xpath(TAB, //tr, TR), 
             findall(Cell,
                     get_cell(TR, Cell),
                     Cell_List
             ),
             Row =.. [table_data|Cell_List]
            ), 
            Whole_Table
    ), 
    Whole_Table = [_|Table_Data]. % skip a header

%
% Parse a table cell
% Return cell as a set of known components
%
% get_cell(+TR, ?Cell)
%

get_cell(TR, Cell) :-

   xpath(TR, td, TD),
   findall(Cell_Element, parse_cell(TD, Cell_Element),
           Element_List),
   list_to_set(Element_List, Cell).

%
% component-recognizers
%
% parse_cell(+TD, ?Cell_Element)
%

parse_cell(TD, TD_Norm) :-

    xpath(TD, /self(normalize_space), TD_Norm),
    TD_Norm \= ''.

parse_cell(TD, onclick(On_Click)) :-
   
    xpath(TD, a(@onclick), On_Click),
    On_Click \= ''.

parse_cell(TD, href(Href)) :-
   
    xpath(TD, a(@href), Href),
    Href \= ''.

% recursion on unknown elements
parse_cell(TD, TD2) :-

    xpath(TD, '*', TDX),
    \+ xpath(TDX, /a, _),
    parse_cell(TDX, TD2).


%% table_v_cast(+DB_Key, +Table, +Class_To, +Col_Specs,
%% +Obj_Fields, +Common_Field_Names, +Common_Field_Values).
%
% Cast table to a business objects and put them into db
%
% @param Table table_v or its descendant (see
% ../../html/v/table_v.pl)
%
% @param Class_To class of bussiness objects (attention: it will
% further downcast it before put in DB)
%
% @param Col_Specs is a list of elements:
%  $ value(<value>) : an implicit value
%  $ <atom> : table column name (single text)
%  $ <integer> : column number (1-based)
%  $ href(Col_Spec) : single href component
%  $ onclick(Col_Spec) : single onclick component
%  $ piece(Col_Spec, Pattern) : the first match only
%  $ then(Spec1, Spec2) : Spec1, if fail - Spec2
%
% @param Obj_Fields list of Class_To fields to put values
% evaluated by Col_Specs
%
% @param Common_Field_Names - names of object fields with the
% same values defined by Common_Field_Values
%
% @param Common_Field_Values - values for Common_Field_Names

table_v_cast(DB_Key, Table, Class_To, Col_Specs, Obj_Fields,
             Common_Field_Names, Common_Field_Values) :-

   Ctx = context(table_v_cast/7, _),
   check_db_key(DB_Key, Ctx),
   check_inst(Table, Ctx),
   check_inst(Class_To, Ctx),
   check_inst(Col_Specs, Ctx),
   check_inst(Obj_Fields, Ctx),
   check_inst(Common_Field_Names, Ctx),
   check_inst(Common_Field_Values, Ctx),
   check_object_arg(Table, Ctx, _),
   check_existing_class_arg(Class_To, Ctx),
   
   check_fields_arg(Obj_Fields, Ctx),
   check_values_arg(Obj_Fields, Col_Specs, Ctx),
   must_be(list(ground), Col_Specs),
   
   check_fields_arg(Common_Field_Names, Ctx),
   check_values_arg(Common_Field_Names, Common_Field_Values, Ctx),

   
   obj_field(Table, header, Header),
   obj_field(Table, rows, Rows),
   
   % find col nums
   maplist(spec_to_table_col_nums(Header), Col_Specs, Col_Nums),
   append(Obj_Fields, Common_Field_Names, Field_Names),
   !,

   (
    member(Row, Rows),
    maplist(arg_bac(Row), Col_Nums, Html_Fields),
    (
     maplist(extract_value, Html_Fields, Col_Specs, Fields)
    ->
     append(Fields, Common_Field_Values, Field_Values),
     obj_construct(Class_To, Field_Names, Field_Values, Obj1),
     obj_downcast(Obj1, Obj),
     db_put_object(DB_Key, Obj, _)
    ;
     write_log(['Fail to parse', Row, 'as', Class_To],
               [logger(html_parse), lf(1)])
    ),
    fail
   ;
    true
   ).

%
% Extract information from a cell by a specification.
% Cell is a list of different interpretation as returned
% by (e.g.) get_cell. 
%

extract_value([Value], Spec, Value) :-

   (atom(Spec); integer(Spec)), !.

extract_value([], Spec, '') :-

   (atom(Spec); integer(Spec)), !.

extract_value(Cell, Spec, Value) :-

   (atom(Spec); integer(Spec)), !,
   include(atomic, Cell, [Value]).

extract_value(Cell, onclick(Spec), Value) :-

   member(onclick(Content), Cell),
   count_el(Cell, onclick(_), 1), !, % single
   extract_value([Content], Spec, Value), !.

extract_value(Cell, href(Spec), Value) :-

   member(href(Content), Cell),
   count_el(Cell, href(_), 1), !, % single
   extract_value([Content], Spec, Value), !.

extract_value(Cell, piece(Spec, Pattern), Value) :-

   extract_value(Cell, Spec, Val1),
   extract_piece(Val1, Pattern, Value).

extract_value(Cell, replace(Spec, Repl_List), Value) :-

   extract_value(Cell, Spec, Val1),
   extract_replace(Val1, Repl_List, Value).

extract_value(Cell, then(Spec1, Spec2), Value) :-

   extract_value(Cell, Spec1, Value) ->
   true ;
   extract_value(Cell, Spec2, Value).

extract_value(_, value(Value), Value).

extract_value(Cell, Spec, _) :-

   write_log(['Unable to extract by the specification `',
              Spec,
              '\' from',
              Cell],
             [lf(1), logger(html_parse)]
            ),
   fail.

extract_piece(Full, Pattern, Match) :-
        
   break_pattern(Pattern, Prefix, Suffix),

   % a prefix can start not from begin but it really defines
   % a start position of a match
   sub_atom(Full, B, L, _, Prefix), !,
   Start is B + L,
   sub_atom(Full, Start, _, 0, Piece1), !,
   (  Suffix = ''
   -> Match = Piece1
   ; sub_atom(Piece1, Len, _, _, Suffix), !,
     sub_atom(Full, Start, Len, _, Match)
   ).

extract_replace(Orig, Repl_List, Replaced) :-

   memberchk(Orig -> Replaced, Repl_List) ->
   true ;
   Replaced = Orig.

break_pattern(Pattern, Prefix, Suffix) :-

   sub_atom(Pattern, Prefix_Len, 1, Suffix_Len, '%'), !,
   sub_atom(Pattern, 0, Prefix_Len, _, Prefix),
   sub_atom(Pattern, _, Suffix_Len, 0, Suffix).

%
% Get column numbers by Col_Spec
%

spec_to_table_col_nums(Table_Header, Col_Spec, Col_Num) :-

   extract_column_def(Col_Spec, Col_Def),
   (
      integer(Col_Def), Col_Def > 0
   ->
      functor(Table_Header, _, N_Cols),
      Col_Def =< N_Cols,
      Col_Num = Col_Def
   ;
      atom(Col_Def),
      arg(Col_Num, Table_Header, Col_Def)
   ).

extract_column_def(Atom, Atom) :- atom(Atom), !.

extract_column_def(Integer, Integer) :- integer(Integer), !.

extract_column_def(onclick(Col_Spec), Col_Def) :-

   extract_column_def(Col_Spec, Col_Def), !.

extract_column_def(href(Col_Spec), Col_Def) :-

   extract_column_def(Col_Spec, Col_Def), !.

extract_column_def(piece(Col_Spec, _), Col_Def) :-

   extract_column_def(Col_Spec, Col_Def), !.

extract_column_def(replace(Col_Spec, _), Col_Def) :-

   extract_column_def(Col_Spec, Col_Def), !.

extract_column_def(then(Col_Spec1, _), Col_Def) :-

   extract_column_def(Col_Spec1, Col_Def), !.

extract_column_def(value(_), 1) :- !.

extract_column_def(X, _) :-

   Ctx = context(extract_column_def/2, _),
   throw(error(domain_error(table_v_column_specification, X),
               Ctx)).
