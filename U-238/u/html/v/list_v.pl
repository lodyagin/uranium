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

  ---+++ Parent
  ../../http/v/http_result_v.pl

  ---+++ New static fields
  * type
  The list type (numbered, unnumbered, definitions)

  * definition_list

  * value_list
*/

:- use_module(library(error)).
:- use_module(u(ur_lists)).

new_class(list_v, http_result_v,
          [http_result_v,
           definition_list,
           value_list
          ]
         ).

'list_v?'(Obj, option_list, List) :-

   obj_field(Obj, definition_list, Defs),
   obj_field(Obj, value_list, Vals),

   (  var(Defs)
   -> true
   ;  \+ is_of_type(list(atom), Defs)
   -> print_message(error, type_error(list(atom), Defs))
   ;  \+ is_of_type(list, Vals)
   -> print_message(error, type_error(list, Vals))
   ;
      length(Defs, Defs_Len),
      length(Vals, Vals_Len),
      Defs_Len \= Vals_Len
   -> print_message(error, domain_error(matched_list_length,
                                        (Defs, Vals)))
   ;
      corteging(=, Defs, Vals, List)
   ).



