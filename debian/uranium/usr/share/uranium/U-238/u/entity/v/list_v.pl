% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
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

:- module(list_v, []).

/** <module> parsed html list

  TODO make it general, remove http_result_v parent

  ---++ list_v

  ---+++ Parent
  ../../http/v/http_result_v.pl

  ---+++ New static fields
   * value_list

  ---++ definition_list_v

  ---+++ Parent
  list_v

  ---+++ New eval fields
   * option_list
   definition = value | definition = [value, ...]

   where definition is the first element of list item, value is
   second etc.
*/

:- use_module(library(error)).
:- use_module(u(v)).

new_class(list_v, http_result_v,
          [value_list
          ]
         ).

new_class(definition_list_v, list_v, []).

'definition_list_v?'(Obj, option_list, List) :-

   obj_field(Obj, value_list, Vals),

   (  var(Vals)
   -> true
   ;  \+ is_of_type(list, Vals)
   -> print_message(error, type_error(list, Vals))
   ;
      lol_options(Vals, [], List)
   ).


lol_options([], L, L) :- !.

lol_options([[_]|T2], L0, L) :- !, % skip single values

   lol_options(T2, L0, L).

lol_options([[Def, Val]|T2], L0, [Def = Val|L1]) :-

   lol_options(T2, L0, L1).

lol_options([[Def|T1]|T2], L0, [Def = T1|L1]) :-

   lol_options(T2, L0, L1).

lol_options([_|T2], L0, L) :- !, % skip single values

   lol_options(T2, L0, L).

