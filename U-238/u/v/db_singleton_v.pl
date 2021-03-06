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

%  This module describes db_singleton_v - the object which
%  always constraints itself to a single instance inside
%  DB.

:- module(db_singleton_v, []).

/** <module> db_singleton_v

  Objects of this class and its descendants (which are not
  overriding the key) can be present in any DB only once.

  ---+++ Parent
  db_object_v.pl

  ---+++ New static fields
  None.
  
  ---+++ New evaluated fields
    $ db_singleton : always evaluated to the object functor

  ---+++ Key
  =|[db_singleton]|=
*/

new_class(db_singleton_v, db_object_v,
          [],
          [db_singleton] % eval field as a key
         ).

% always return functor
'db_singleton_v?'(Obj, db_singleton, Functor) :-

   functor(Obj, Functor, _).


                  