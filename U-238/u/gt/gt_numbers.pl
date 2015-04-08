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
   random_options(random_number, Options, Det, Generator, Seed0, Seed, Opt),
   obj_field(Opt, domain, Domains),
   random_member(Domain, Domains, Det, Generator, Seed0, Seed1),
   (  Domain == integer
   -> random_number_integer(Opt, Det, Generator, Seed1, Seed, Num)
   % TODO implement others
   ;  must_be(oneof([integer, rational, real]), Domain)
   ).

random_number_integer(Opt, Det, Generator, Seed1, Seed, Num) :-
   obj_field(Opt, pattern, Patterns),

   % NB Patterns is always non-empty due to defaults
   random_member(Pattern1, Patterns, Det, Generator, Seed1, Seed2),

   (  Pattern1 = range(Drep)
   -> context_module(That),
      Pattern = That:range_pattern(Drep)
   ;  pattern(Pattern) = Pattern1,
      must_be(callable, Pattern)
   ),
   call(Pattern, Num),
   fd_random(Generator, Seed2, Seed, Num),
   (  Det == semidet -> ! ; true ).


range_pattern(Set, Num) :-
  Num in Set.


