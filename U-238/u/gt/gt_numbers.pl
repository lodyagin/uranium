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

% This module contain various predicaties
% for random numbers generation.

:- module(gt_numbers,
          [random_number/1,
	   random_number/2
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(rand/randgen)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

random_number(Number) :-

   Ctx = context(random_number/1, _),
   random_number_cmn([], Number, Ctx).

:- meta_predicate random_number(:, -).

random_number(Options, Number) :-

   Ctx = context(random_number/2, _),
   random_number_cmn(Options, Number, Ctx).

random_number_cmn(Options, Num, _) :-
   options_to_object(random_number, Options, Opt),
   obj_field(Opt, domain, Domains),
   random_member(Domain, Domains),
   (  Domain == integer
   -> random_number_integer(Opt, Num)
   % TODO implement others
   ;  must_be(oneof([integer, rational, real]), Domain)
   ).

random_number_integer(Opt, Num) :-
   obj_unify(Opt, weak,
             [pattern,
              generator,
              seed
              ],
             [Patterns,
              generator(Generator),
              Seed_Opt
             ]),

   must_be(callable, Generator),
   must_be(list, Patterns),
   memberchk(Seed_Opt, [seed(Seed0), seed(Seed0, Seed)]),
   must_be(integer, Seed0),
   
   % TODO always use passed generators instead of library(random)
   random_member(Pattern1, Patterns),

   must_be(callable, Pattern1),

   (  Pattern1 = range(Drep)
   -> context_module(That),
      Pattern = That:range_pattern(Drep)
   ;  pattern(Pattern) = Pattern1
   ),
   (  Seed0 >= 0
   -> Seed1 = Seed0
   ;  Seed1 is random(4294967295) % TODO
   ),
   call(Pattern, Num),
   fd_random(Generator, Seed1, Seed, Num), !.


range_pattern(Set, Num) :-

  Num in Set.


