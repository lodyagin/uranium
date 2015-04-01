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
:- use_module(library(clpfd)).
:- use_module(u(ur_option)).
:- use_module(u(gt/gt_numbers)).

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


typedef(nonneg, [value_set - nonneg_set_gen]).

:- meta_predicate nonneg_set_gen(:, -).

nonneg_set_gen(Options, Value) :-

   options_object(nonneg_set_gen, Options, Opt),

   obj_unify(Opt,
             [range, generator, seed],
             [range(From, To), generator(Gen), seed(Seed0)]),

   (  Seed0 < 0
   -> Seed is random(4 * 10**9)
   ;  Seed = Seed0
   ),
   Value in From .. To,
   call(Gen, Seed, _, Value).

:- meta_predicate v_gen(+, :, -).

v_gen(Class, Options, Obj) :-
   options_object(global:Class, Options, Options1),
   obj_construct(Class, [], [], Obj0),
   obj_fill_downcast_random(Options1, Obj0, Obj).

:- meta_predicate list_member_gen(+, :, -).

list_member_gen(Field, Options, Value) :-
   options_object(global:Field, Options, Opt),
   obj_field(Opt, list, Types),
   random_member(Value, Types). %TODO proper gen and seed
   
:- meta_predicate vs_gen(+, :, -).

% Generates list of homogenous objects of Class
vs_gen(Field, Class, Options, Objs) :-
   options_object(global:Field, Options, Opt),
   obj_field(Opt, length, Lengths).
   % Convert options
%   findall(range(Range), member(length(Range), Lengths), Ranges),
% ?  append(Ranges, Options, Options1),
%   random_number([rangeOptions, 
  








