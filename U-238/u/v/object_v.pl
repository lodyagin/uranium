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
:- use_module(u(rand/randgen)).
:- use_module(u(gt/gt_numbers)).
:- use_module(u(logging)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_enums)).

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
typedef(string, [pretty_print - string_pretty_print(string)]). % TODO string

:- meta_predicate nonneg_set_gen(:, -, -).

nonneg_set_gen(Options0, Options, Value) :-
   options_to_object(nonneg_set_gen, Options0, Options1),
   obj_field(Options1, range, range(From, To)),
   Value in From .. To,
   random_number(Options1, Options, Value).

:- meta_predicate v_gen(+, :, -, -).

v_gen(Class, OM:Opts, OM:Options, Obj) :-
   options_to_object(global:Class, Opts, Options0),
   obj_rewrite(Options0, [global_options], [GO0], [GO2], Options1),
   (   var(GO0) -> obj_construct(global_options_v, [], [], GO1)
   ;   GO1 = GO0
   ),
   obj_rewrite(GO1, [log_options], [LogOpts0], [LogOpts2], GO2),
   (  nonvar(LogOpts0) -> LogOpts1 = LogOpts0 ; LogOpts1 = [] ),
   log_piece(['v_gen(', Class, ', ..., ...)'], LogOpts1),
   change_indent(LogOpts1, LogOpts2, 2),
   (   var(Obj)
   ->  obj_construct(Class, [], [], Obj0),
       obj_fill_downcast_random(Options1, Options2, Obj0, Obj)
   ;
       obj_fill_random(Options1, Options2, Obj)
   ),
   obj_rewrite(Options2, [global_options], [GO3], [GO4], Options),
   obj_rewrite(GO3, [log_options], [LogOpts3], [LogOpts4], GO4),
   change_indent(LogOpts3, LogOpts4, -2).

:- meta_predicate list_member_gen(+, :, -, -).

% TODO add Class parameter
list_member_gen(Field, OM:Options0, OM:Options, Value) :-
   (   nonvar(Value) -> Options = Options0
   ;
   options_to_object(global:Field, Options0, Options1),
   Options1 // global_options // log_options ^= LogOpts,
   (   nonvar(LogOpts) -> LogOpts1 = LogOpts ; LogOpts1 = [lf(1)] ),
   log_piece(['list_member_gen(', Field, ', ..., ...)'], LogOpts1),
   (  random_options(Options1, Options, Det, Gen, Seed0, Seed, phase_match)
   -> obj_field(Options1, list, Types),
      random_member(Value, Types, Det, Gen, Seed0, Seed)
   ;  Options0 = Options
   )
   ).

:- meta_predicate enum_member_gen(+, :, -, -).

enum_member_gen(Field, OM:Options0, OM:Options, enum(Enum, Integer)) :-
   options_to_object(global:Field, Options0, Options1),
   functor(Options1, OptionsClass, _),
   (   ( nonvar(Integer) ; nonvar(Enum) ) 
   ->  enum_integer(OptionsClass:Enum, OptionsClass:Integer),
       Options = Options1
   ;
   Options1 // global_options // log_options ^= LogOpts,
   (   nonvar(LogOpts) -> LogOpts1 = LogOpts ; LogOpts1 = [lf(1)] ),
   log_piece(['list_member_gen(', Field, ', ..., ...)'], LogOpts1),
   (  random_options(Options1, Options, Det, Gen, Seed0, Seed, phase_match)
   -> (  enum_size(OptionsClass:N)  % OptionsClass as a pseudo module
      -> true % this enum is registered already
      ;  options_group_list(global:Field, list, Enums),
         assert_enum(OptionsClass:Enums),
         length(Enums, N)
      ),
      succ(N1, N),
      Integer in 0..N1,
      fd_random(LogOpts, Gen, Seed0, Seed, Integer),
      (  Det == semidet -> ! ; true ),
      enum_integer(OptionsClass:Enum, OptionsClass:Integer), !
   ;
      Options0 = Options
   )
   ).

:- meta_predicate vs_gen(+, +, :, -, -).

% Generates list of homogeneous objects of Class
vs_gen(Field, Class, OM:Options0, OM:Options, Objs) :-
   options_to_object(global:Field, OM:Options0, Options1),
   Options1 / global_options / log_options ^= LogOpts,
   log_piece(['vs_gen(', Field, ', ', Class, ', ..., ...)'], LogOpts),
   (   var(Objs)
   ->  % generate a list of random size
       options_predicate_to_options_class_name(global:Field, OptClass1),
       options_predicate_to_options_class_name(gt_numbers:random_number, OptClass2),
       obj_rebase((OptClass1 -> OptClass2), Options1, Options2),
       % Convert options length(Range) -> range(Range)
       obj_field(Options1, length, Lengths),
       obj_unify(Options2, [pattern, domain], [Ranges, [integer]]),
       findall(range(Range), member(length(Range), Lengths), Ranges),
       % Get the number of objects
       random_number(Options2, Options3, N),
       % Copy fields left in Options1
       obj_rebase((OptClass2 -> OptClass1), Options3, Options4),
       unbounded_fields(Options4, FieldsToRestore),
       obj_unify(Options1, FieldsToRestore, ValuesToRestore),
       obj_unify(Options4, FieldsToRestore, ValuesToRestore),
       % Generate N random objects
       bagof(V,
             L^( length(L, N),
                 member(V, L),
                 obj_construct(Class, [], [], V)
               ),
             Objs0),
       obj_fill_downcast_random_list(Options4, Options, Objs0, Objs)
   ;
       obj_fill_random_list(Options1, Options, Objs)
   ).

string_pretty_print(Field, _, Value, Options) :-
   atom_string(Atom, Value),
   log_piece([Field, ':', Atom], Options).

