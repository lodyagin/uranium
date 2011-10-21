%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

% Template for parsing html tables (row -> object)

:- module(find_all_table_rows, [find_all_table_rows/5]).

:- use_module(u(vd)).
:- use_module(parser/html/html_page_parse).
:- use_module(parser/html/html_page_find).
:- use_module(parser/html/table_v_parse).

find_all_table_rows(DB_Key, Page, Table_Search_Columns, Class,
                    Cast_Expr) :-

    % extract all tables from the page
    atom_concat(DB_Key, '.all_tables', DB2_Key),
    db_clear(DB2_Key),
    html_page_parse(DB2_Key,
                    Page,
                    [table_v]),
    
    dump_db(DB2_Key),

    % find tables
    atom_concat(DB_Key, '.match_tables', DB3_Key),
    db_clear(DB3_Key),
    html_page_find(DB2_Key, DB3_Key, table_v,
                   % treat a table as a matched table
                   % if it contains these columns
                   Table_Search_Columns),
    
    dump_db(DB3_Key),

    % concat all found tables and make objects
    (
     db_recorded(DB3_Key, Table),

     Cast_Expr = cast_expr(In_Table, In_Object, Common_Names, Common_Values),

     table_v_cast(DB_Key, Table, Class,
                  In_Table, In_Object, Common_Names, Common_Values),
     fail
    ;
     true
    ),

    dump_db(DB_Key).

