% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(html_page_parse,
          [html_page_parse/3
          ]).

/** <module> Parse page_v objects.

  Extract specified classes from ../../html/v/page_v.pl
*/

:- use_module(u(vd)).
:- use_module(u(v)).
:- use_module(u(ixpath)).
:- use_module(u(internal/check_arg)).


%% html_page_parse(+DB_Key, +DOM, +Elements_To_Extract)
%
% Parse DOM of html page and create a list of html_tag_v
% descendants.
%
% @param DB_Key where to put objects
% @param DOM it should be object with `dom' field (page_v)
% @param Elements_To_Extract a list of object names, see
% element_type_tag/3.

html_page_parse(DB_Key, Page, Elements_To_Extract) :-

  Ctx = context(html_page_parse/3, _),
  check_db_key(DB_Key, Ctx),
  check_object_arg(Page, Ctx, _),
  check_list_fast_arg(Elements_To_Extract, Ctx),
  list_to_set(Elements_To_Extract, Elements_Set),
  maplist(extract_elements(DB_Key, Page), Elements_Set).

extract_elements(DB_Key, Page, Class) :-

  db_put_objects(DB_Key,
		 extract_element(Page, Class),
		 ignore).


% extract_element(+Page, +Class, -Object) is nondet.
extract_element(Page, Class, Object) :-
  obj_field(Page, www_address, WWW_Address),
  element_type_tag(Class, Tag),
  ixpath(//Tag, [vixc], Page, Object1),
  %atom_concat(Cmn_Class, '_parse', Pred),
  %atom_concat('parser/html/', Pred, Module),
  %use_module(u(Module), [Pred/2]),
  %debug(html_page_parse, 'Call ~a for ~p', [Pred, Object1]),
  %call(Pred, Object1, Object2),
  obj_unify(Object1,
            [www_address, root_node],
            [WWW_Address, Page]),
  % TODO move root_node setting into ixpath
  gtrace,
  obj_downcast(Object1, Object),
  once(( functor(Object, Class, _)
       ; obj_is_descendant(Object, Class)
       )).


% element_type_tag(?Class, ?Tag) is nondet.
% True if html_tag_<Tag>_v can be downcasted to Class.
element_type_tag(Class, Tag) :-
   class_parent(Tag_Class, html_tag_v),
   (  Tag_Class = Class
   ;  class_can_downcast(Tag_Class, Class)
   ),
   concat_atom([html, tag, Tag, v], '_', Tag_Class).
