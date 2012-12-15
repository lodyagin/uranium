% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
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

:- module(table_v, []).

/** <module> parsed html table

  ---+++ Parent
  ../../http/v/http_result_v.pl

  ---+++ New static fields
  * header
  table_header(H1, H2, ...) predicate (arity = number of columns)

  * rows
  list of table_row(R1, R2, ...) predicates
*/


% header - table_header(...)
% rows - [table_data(...)]
%

% TODO html_tag_td_v must be parent
new_class(table_v, html_tag_v, [header, rows]).

