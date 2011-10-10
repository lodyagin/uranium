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

%
% Find objects on the parsed html page
%

:- module(html_page_find, [html_page_find/4]).

:- use_module(library(ur_objects)).
:- use_module(library(ur_recorded_db)).

%
% html_page_find(+DB_Key, +Class, +Search_Criteria, ?Obj_List) :-
%


% Все запрошенные колонки должны присутствовать

html_page_find(DB_In, DB_Out, table_v, Column_List) :-

    db_search(DB_In, DB_Out, table_match(Column_List)), !.

table_match(Column_List, Table) :-

    obj_field(Table, header, Header),
    maplist(table_header_match(Header), Column_List).

table_header_match(Header, Column) :-

    arg(_, Header, Column), !.

