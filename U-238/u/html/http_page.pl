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


:- module(http_page,
          [http_page/3,  % :Pred, +URL, -Page
           http_page/4   % :Pred, +URL, @Form, -Page
          ]).

:- use_module(u(v)).
:- use_module(library(error)).
:- use_module(library(url)).

:- meta_predicate http_page(3, +, -).
:- meta_predicate http_page(3, +, +, -).

% Get an http response as a html_piece_v descendant
%
% http_page(:Pred, +URL, -Page)
%

http_page(Pred, URL, Page) :-

   Ctx = context(http_page/3, _),
   http_page_cmn(Pred, URL, _, Page, Ctx).

http_page(Pred, URL, Form, Page) :-

   Ctx = context(http_page/4, _),
   http_page_cmn(Pred, URL, Form, Page, Ctx).

http_page_cmn(Pred, URL, Form, Page, _) :-

  must_be(callable, Pred),
  call(Pred, URL, Form, DOM),
  get_time(Timestamp),

  (atom(URL) -> URL2 = URL;  parse_url(URL2, URL)),

  (  DOM = [DOM2] -> true ; DOM = DOM2 ),
  
  obj_construct(html_piece_v,
                [http_request_url, timestamp, dom],
                [URL2, Timestamp, DOM2],
                Obj),
  obj_downcast(Obj, Page).
  