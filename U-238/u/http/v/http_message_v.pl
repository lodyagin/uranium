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

:- module(http_message_v, []).

/** <module> Common parent for rfc2616 request and response.

  ---+++ Parent
  v(object_v)

  ---+++ New static fields

   * http_version_major
   * http_version_minor
  
   * headers
   Instance of v(http_headers_v).

   * message_body

  ---+++ New evaluated fields

   * http_version
   HTTP version as an atom, e.g. '1.1'. Only accepts
   the structure version/2 as an input:
   =|version(1, 1)|=
*/

:- use_module(u(v)).

new_class(http_message_v, object_v,
          [http_version_major,
           http_version_minor,
           headers,
           message_body]).

          
'http_message_v?'(Obj, http_version, Atom) :-

   var(Atom), !,
   obj_unify(Obj,
             [http_version_major, http_version_minor],
             [Major, Minor]),
   (  (var(Major) ; var(Minor)) -> true
   ;  format(atom(Atom), '~d.~d', [Major, Minor])
   ).

'http_message_v?'(Obj, http_version, Version) :-

   compound(Version), !,
   Version = version(Major, Minor),
   obj_unify(Obj,
             [http_version_major, http_version_minor],
             [Major, Minor]).

             

