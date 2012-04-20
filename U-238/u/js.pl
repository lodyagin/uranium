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

:- module(js,
          [js_decode_string/2
          ]).

:- use_module(library(http/dcg_basics)).

js_decode_string(Str_In, Str_Out) :-

  atom_codes(Str_In, Codes),
  phrase(js_string(Str), Codes),
  atom_codes(Str_Out, Str).

js_string(Str) -->

  string_without("\\", Part1),
  (  hex_symbol(Hex)
  -> js_string(Part2),
     { append(Part1, [Hex|Part2], Str) }
  ;  { Str = Part1 }
  ).

hex_symbol(Hex) -->

  "\\x",
  xdigit(H1),
  xdigit(H2),
  { Hex is H1 * 16 + H2 }.
