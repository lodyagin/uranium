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


:- module(http_page, [http_page/3]).

:- use_module(library(ur_objects)).
:- use_module(library(url)).

% Get an http response as a html_piece_v descendant
%
% http_page(+URL, ?Page)
%

http_page(URL, Pred, Page) :-

  call(Pred, URL, DOM),
  get_time(Timestamp),

  (atom(URL) -> URL2 = URL;  parse_url(URL2, URL)),
  
  obj_construct(html_piece_v,
                [http_request_url, timestamp, dom],
                [URL2, Timestamp, DOM],
                Obj),
  obj_downcast(Obj, Page).
  