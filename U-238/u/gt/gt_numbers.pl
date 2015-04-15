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
          [random_number/1, % -Number
	   random_number/3  % +Options0, -Options, -Number
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(rand/randgen)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

random_number(Number) :-
   Ctx = context(random_number/1, _),
   random_number_cmn([], _, Number, Ctx).

:- meta_predicate random_number(:, -, -).

%% random_number(+Options0, -Options, -Number) is nondet.
%
%  Generates a random number for clpfd constrained Number.
%  Example: X in 1..10, random_number([semidet], _, X).
%  It is semidet if the semidet option passed.
%
random_number(OM:Options0, Options, Number) :-
   Ctx = context(random_number/3, _),
   (  nonvar(Options), Options = OM:Options1
   -> true
   ;  Options = Options1
   ),
   random_number_cmn(OM:Options0, Options1, Number, Ctx).

random_number_cmn(OM:Options0, Options, Num, _) :-
   (  nonvar(Num) -> Options0 = Options
   ;
   options_to_object(random_number, OM:Options0, Options1),
   (  random_options(Options1, Options, Det, Generator, Seed0, Seed, phase_match)
   -> obj_field(Options, domain, Domains),
      random_member(Domain, Domains, Det, Generator, Seed0, Seed1),
      (  Domain == integer
      -> random_number_integer(Options1, Det, Generator, Seed1, Seed, Num)
      % TODO implement others
      ;  must_be(oneof([integer, rational, real]), Domain)
      )
   ; Options = Options0
   )
   ).

random_number_integer(Opt, Det, Generator, Seed1, Seed, Num) :-
   obj_field(Opt, pattern, Patterns),

   % NB Patterns are always non-empty due to defaults
   random_member(Pattern1, Patterns, Det, Generator, Seed1, Seed2),

   (  Pattern1 = range(Drep)
   -> context_module(That),
      Pattern = That:range_pattern(Drep)
   ;  pattern(Pattern) = Pattern1,
      must_be(callable, Pattern)
   ),
   call(Pattern, Num),
   LogOpts ^= Opt / global_options / log_options,
   fd_random(LogOpts, Generator, Seed2, Seed, Num),
   (  Det == semidet -> ! ; true ).


range_pattern(Set, Num) :-
  Num in Set.


