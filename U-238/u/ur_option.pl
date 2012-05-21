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

:- module(ur_option,
          [ur_options/2,
           options_object/3
           ]).

/** <module> Options processing
*/

:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(v(ur_options_v)).

:- meta_predicate ur_options(:, :).

%% ur_options(:Predicate, +Options)
%
% Define the options rule for Predicate
%
% Example:
% ==
% ur_options(random_string,
%           [multi(length, [length/2, length/1, empty/0], [empty, length(1, 80)]),
%            single(pattern, [range/1, regex/1, _:pattern], range(32..126)),
%            option(_:generator, randgen:fd_random(lcq, gnu))
%           ]).
% ==

ur_options(Pred, _:Options) :-

   Ctx = context(ur_options/3, Details),
   strip_module(Pred, Pred_Module, Pred1),
   format(atom(Class), '~a__~a_v', [Pred_Module, Pred1]),
   ( class_name(Class) -> true
   ; assert_rules(Options, Class, Ctx, Details),
     setof(Field,
           Pattern^Arity^
           ( db_select(Class, [pattern], [Pattern]),
             functor(Pattern, Field, Arity)
           ),
           Fields),
     class_create(Class, ur_options_v, Fields)
   ).

assert_rules([], _, _, _) :- !.
assert_rules([Rule0|T], DB, Ctx, Details) :-
   Err = error(invalid_option_definition(Rule0), Ctx),
   must_be(list, Rule0),
   select_option(option(Option), Rule0, Rule1),
   (  nonvar(Option), Option = Functor/Arity -> true
   ;  Details = 'option(Functor/Arity) expected',
      throw(Err)
   ),
   (  Rule1 == [] -> true
   ;  throw(Err)
   ),
   assert_rule(DB, Functor, Arity),
   assert_rules(T, DB, Ctx, Details).

assert_rule(DB, Functor, Arity) :-
   functor(Pattern, Functor, Arity),
   db_construct(DB, single_option_rule_v, [pattern], [Pattern]).

:- meta_predicate options_object(:, :, -).

options_object(Module:Pred, _:Options, Object) :-
   format(atom(Class), '~a__~a_v', [Module, Pred]),
   obj_construct(Class,
                 [options_in, options_out],
                 [Options, Object],
                 _).

% override_(+Option0, +Value, +Options, -Option)
%
% @param Option0 can have one of these forms:
%  $ default(Opt) : the default value for Opt
%  $ default_to_multi(List) : the default for muli-option
%  $ user(Opt) : user defined value for Opt
%  $ multi(List) : list of options of one group
%   (like [empty, length(4), length(6, 7)]).
%
% @param Value processed option (like length(1, 5))
%
% @param Options the full option list (for error reporting)
%
% @param Option will be one of [user(Opt), multi(List)]

% process opt(match(Val)) expression, Val will be unified with
% non-multy _(opt(Val)) option,
/*
override_(Prev_Stu, Stu, _, Prev_Stu) :-
	functor(Prev_Stu, _, 1),      %e.g. user(opt(Val))
	functor(Stu, _, 1),           %e.g. opt(match(Val))
	arg(1, Stu, match(Val)),
	!,
	arg(1, Prev_Stu, Opt),
	functor(Opt, _, 1),
	arg(1, Opt, Val).

override_(default(_), Value, _, user(Value)) :- !.
override_(user(Prev), Value, Options, _) :- !,
        (   Value == Prev ->
            domain_error(nonrepeating_options, Options)
        ;   domain_error(consistent_options, Options)
        ).
override_(default_to_multi(_), Value, _, multi([Value])) :- !.
override_(multi([]), Value, _, multi([Value])) :- !.
override_(multi([V1|T]), Value, _, multi([Value, V1|T])) :- !.
*/












