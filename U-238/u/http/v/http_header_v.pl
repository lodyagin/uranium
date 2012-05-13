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

new_class(http_header_content_type_v, http_header_v,
          [type,
           subtype,
           parameters]).

'http_header_v?'(Obj, class, Class) :-

   obj_field(Obj, name, Name),
   concat_atom([http_header, Name, v], '_', Class1),
   (  class_name(Class1)
   -> Class = Class1
   ;  Class = http_header_v
   ).

TODO: rfc822 not use diff lists if not neccessary
      rfc2616 here :  Linear white space
   (LWS) MUST NOT be used between the type and subtype, nor between an
   attribute and its value

downcast(http_header_v, http_header_content_type_v, From, To) :-

   gtrace,
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
         