% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose
% functional test platform.
%
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it
% and/or modify it under the terms of the GNU Lesser
% General Public License as published by the Free Software
% Foundation; either version 2.1 of the License, or (at
% your option) any later version.
%
% This library is distributed in the hope that it will be
% useful, but WITHOUT ANY WARRANTY; without even the
% implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser
% General Public License along with this library; if not,
% write to the Free Software Foundation, Inc., 51 Franklin
% Street, Fifth Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(http_header_v, []).

/** <module> Single HTTP header.
*/

:- use_module(u(v)).
:- use_module(u(internet/rfc2045)).

new_class(http_header_v, object_v, [name, body]).

new_class(http_invalid_header_v, http_header_v, []).

new_class(http_header_content_type_v, http_header_v,
          [type,
           subtype,
           parameters]).

'http_header_v?'(Obj, class, Class) :-

   obj_unify(Obj, [name, body], [Name, Body]),
   concat_atom([http_header, Name, v], '_', Class1),
   (  class_name(Class1)
   -> (  var(Body)
      -> Class = Class1
      ;  check_syntax(Class1, Body)
      -> Class = Class1
      ;  Class = http_invalid_header_v
      )
   ;  Class = http_header_v
   ).

'http_header_v?'(Obj, name_value, NV) :-

   obj_unify(Obj, [name, body], [Name, Value]),
   (  var(Name) -> true
   ;  NV = (Name = Value)
   ).

check_syntax(http_header_content_type_v, Body) :-
   atom_codes(Body, Codes),
   phrase(media_type(_, _, _), Codes).

downcast(http_header_v, http_header_content_type_v, From, To) :-

   obj_field(From, body, Body),
   (  nonvar(Body)
   ->
      atom_codes(Body, Codes),
      phrase(media_type(Type, Subtype, Parameters), Codes),
      obj_unify(http_header_content_type_v,
                [type, subtype, parameters],
                [Type, Subtype, Parameters], To)
   ;  true
   ).

