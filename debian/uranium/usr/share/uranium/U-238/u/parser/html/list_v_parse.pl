% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(list_v_parse,
          [list_v_parse/2
           ]).

/** <module> Parse HTML lists
  */

:- use_module(u(v)).
:- use_module(library(xpath)).

list_v_parse(DOM, Object) :-

   findall(Line,
           (  xpath(DOM, li, LI),
              LI = element(li, _, Line)
           ),
           Value_List0),
   maplist(normalize_values, Value_List0, Value_List),
   obj_construct(list_v, [value_list], [Value_List], Object).

normalize_values(V0, V) :-

   normalize_values(V0, [], V).

normalize_values([], L, L) :- !.

normalize_values([element(Tag, Attrs, Vals)|T], L0, [V|L1]) :- !,

   normalize_value(Tag, Attrs, Vals, V),
   normalize_values(T, L0, L1).

normalize_values([V|T], L0, [V|L1]) :-

   normalize_values(T, L0, L1).


normalize_value(span, _, [V], V) :- !.

normalize_value(span, _, L, V) :- !,

   normalize_values(L, [], V).

normalize_value(_, _, [V], V) :- !.

normalize_value(_, _, L, V) :- !,

   normalize_values(L, [], V).







