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

:- module(msdn_country, []).

:- use_module(u(v)).
:- use_module(u(ur_recorded_db)).
:- use_module(u(ur_lists)).
:- use_module(u(html/http_page)).
:- use_module(u(html/http_ops)).
:- use_module(parser/html/html_page_parse).
:- use_module(parser/html/html_page_find).
:- use_module(parser/html/table_v_parse).
:- use_module(parser/msdn/parse_msdn_list).

:- use_module(u(action/templates/find_all_table_rows)).

find_all_countries(DB_Key, User) :-

    obj_field(User, cookie_db_key, Cookies_DB),

    ground(Cookies_DB),

    http_page('http://msdn.microsoft.com/en-us/library/cdax410z.aspx',
              http_ops:http_get_html([], Cookies_DB),
              Page
             ),

    find_all_table_rows:find_all_table_rows(DB_Key,
			Page,
			['country/region'],
			country_v,
			cast_expr(['country/region', 
				   'country/region_string'],

				  [country, country_strings],

				  [], []
			)
			),

    % fix the language_strings
    db_iterate_replace(DB_Key, parse_country_strings, parsed(+free)).

% Replace string with list of entities
parse_country_strings(Obj_In, Obj_Out, _) :-

  obj_field(Obj_In, country_strings, String),
  parse_msdn_list(String, List),
  obj_reset_fields([country_strings], Obj_In, Obj_Out, _),
  obj_field(Obj_Out, country_strings, List),
  obj_field(Obj_Out, parsed, true).

    


