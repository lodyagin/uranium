%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(html_tag_html_v, []).

:- use_module(u(ixpath)).
:- use_module(u(v)).


new_class(html_tag_html_v, html_tag_v, []).

'html_tag_html_v?'(Obj, class, Class) :-

   (  ixpath(child::body, Obj, _)
   -> Class = page_v
   ;  true ).


downcast(html_tag_html_v, page_v, From, To) :-

   (  ixpath(child::head/child::title, From, Title)
   -> obj_field(To, title, Title)
   ;  true ).
