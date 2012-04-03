%  -*- fill-column: 65; -*-
% 
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2012, Kogorta OOO Ltd.
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

%  This contains printing rules for all messages in Uranium.

:- module(ur_messages, []).

:- use_module(u(v)).

:- multifile prolog:message//1.

prolog:message(ignore_cookie_attribute(Attr, Val, Obj)) -->

   { obj_field(Obj, set_cookie, Set_Cookie) },
   ['Ignore [~a=~a] for ~p' - [Attr, Val, Set_Cookie]].
