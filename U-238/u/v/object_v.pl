%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
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

%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(object_v, []).

/** <module> object_v

  It is a recomended base class for all Uranium objects (see v.pl).

  Naturally the object_base_v class is the base of any object
  including object_v. But many user-defined predicates can demand
  properties defined in object_v by any object, so it is wise
  to inherit all objects from object_v or its descendants.

  ---+++ Parent
  object_base_v

  ---+++ New static fields

  No static fields.

  ---+++ New evaluated fields

   $ functor : It is a functor of the object. *|Must not be
   overrided in descendants|*.
   $ class : It is the same as _functor_ in _|object_v|_. But it can be
   overrided, see _|Uranium Book, The Downcast|_.

  ---+++ Key

  No key.

  ---+++ Copy operation

  The object_v copy (and, by default, all its descendats, till
  overrided) is performed as duplicate_term/2.
*/

:- use_module(u(v)).

%
% the class evaluation can be overrided
%

'object_v?'(Term, class, Class) :-

   var(Class), !,
   functor(Term, Class, _).

'object_v?'(Term, class, Class) :-

   obj_field(Term, class, Real_Class),
   Class = Real_Class.

%
% the functor evalutation should not be overrided
%

'object_v?'(Term, functor, Class) :-

  functor(Term, Class, _).

copy(object_v, From, To) :-

   duplicate_term(From, To), !.

new_class(object_v, object_base_v, []).
