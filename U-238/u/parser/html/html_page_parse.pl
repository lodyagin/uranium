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

:- module(html_page_parse,
          [html_page_parse/3
          ]).

:- use_module(u(vd)).
:- use_module(u(v)).
:- use_module(library(xpath)).
:- use_module(u(logging)).

%
% Parse DOM of html page and create a list of objects
%
% html_page_parse(+DB_Key, +DOM, +Elements_To_Extract)
%

html_page_parse(DB_Key, Page, Elements_To_Extract) :-

  atom(DB_Key),
  is_list(Elements_To_Extract),
  list_to_set(Elements_To_Extract, Elements_Set),
  maplist(extract_elements(DB_Key, Page), Elements_Set).

extract_elements(DB_Key, Page, Class) :-

  db_put_objects(DB_Key,
		 extract_element(Page, Class),
		 ignore).


% extract_element(+Page, +Class, -Object)
extract_element(Page, Class, Object) :-

  obj_field(Page, dom, DOM),
  ground(DOM),
  element_type_tag(Class, Tag),
  xpath(DOM, //Tag, Data),
  atom_concat(Class, '_parse', Pred),
  atom_concat('parser/html/', Pred, Module),
  use_module(u(Module), [Pred/2]),
  write_log(['Call ', Pred], [logger(html_page_parse)]),
  call(Pred, Data, Object1),

  named_args_unify(Page, [http_request_url, timestamp],
                   [Obj_Url, Obj_Timestamp]),
  named_args_unify(Object1, [http_request_url, timestamp],
                   [Obj_Url, Obj_Timestamp]),

  obj_downcast(Object1, Object).


% object - tag mapping
element_type_tag(table_v, table).
element_type_tag(form_v, form).
element_type_tag(link_v, a).
