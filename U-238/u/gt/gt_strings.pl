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
% for random strings generation.

:- module(gt_strings,
          [random_string/1,
	   random_string/2,
	   range_pattern/2
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(library(random)). % TODO always use passed generators
:- use_module(u(regex/regex)).
:- use_module(u(rand/randgen)).
:- use_module(u(ur_option)).

random_string(Str) :-

   Ctx = context(random_string/1, _),
   random_string_cmn([], Str, Ctx).

:- meta_predicate random_string(:, -).

random_string(Options, Str) :-

   Ctx = context(random_string/2, _),
   random_string_cmn(Options, Str, Ctx).

random_string_cmn(Options, Str, _) :-
   
   options_object(random_string, Options, Opt),

   % check options (TODO: move to ur_options_v)
   obj_unify(Opt,
             [generator,
              seed,
              pattern],
             [generator(Generator),
              seed(Seed),
              Pattern]
            ),
   must_be(callable, Generator),
   must_be(callable, Pattern),
   must_be(integer, Seed),
   
   random_string_int(Opt, Str).


random_string_int(Opt, Str) :-

   obj_unify(Opt, weak,
             [length,
              pattern,
              generator,
              seed
              ],
             [Lengths,
              Patterns,
              generator(Generator),
              seed(Seed0)
             ]),

   % TODO always use passed generators instead of library(random)
   % NB Lengths and Patterns are always non-empty due to defaults
   random_member(Length, Lengths),
   random_member(Pattern1, Patterns),

   % regex/1, range/1 -> pattern/1
   context_module(That),
   (  Pattern1 = regex(Reg)
   -> regex_pattern(Reg, Pattern)
   ;  Pattern1 = range(Drep)
   -> Pattern = That:range_pattern(Drep)
   ;  pattern(Pattern) = Pattern1
   ),

   (  Seed0 >= 0
   -> Seed = Seed0
   ;  Seed is random(4294967295) % TODO
   ),

   random_string_int(Pattern, Generator, Seed, Length,
                     Codes),
   (   nonvar(Str), Str = atom(Atom)
   ->  atom_codes(Atom, Codes)
   ;   Str = Codes
   ).

random_string_int(Pattern, Generator, Seed, Length, Str) :-

   choose_length(Generator, Length, N),
   (  N =:= 0
   ->
      Str = []
   ;
      % Generate a string equation
      length(Str, N),
      call(Pattern, Str),

      % Make a selection
      randsel(Str, Generator, Seed, _)
  ).

% TODO make recursive with a seed through-passing
randsel([], _, Seed, Seed) :- !.

randsel([X|L], Gen, Seed0, Seed) :-

  fd_size(X, Size),
  Size > 0,
  call(Gen, Seed0, Seed1, X),
  randsel(L, Gen, Seed1, Seed).


% Randomly choose length with proper distribution
choose_length(_, empty, 0) :- !.
choose_length(_, length(N), N) :- !.
choose_length(_, length(From, To), N) :-
   % TODO use Gen
   N is From + random(To - From + 1).

range_pattern(Drep, Str) :-

  Str ins Drep.


% Linear distribution over X domain

% regex -> NFA

regex_pattern(Regex, That:regex_pat(Nodes, Arcs)) :-

   regex_dfa(Regex, dfa(States0, Arcs, Initial, Final)),
   !,
   context_module(That),
   sort(States0, States),
   length(States, NStates),
   numlist(1, NStates, States),
   findall(sink(X), member(X, Final), L2),
   Nodes = [source(Initial)|L2].

regex_pat(Nodes, Arcs, Str) :-
   automaton(Str, Nodes, Arcs).


setup_options :-
   
   ur_options(random_string,
              [[multi_group(length), %not_empty,
                option(empty/0), option(length/1), option(length/2),
                default([empty, length(1, 80)])],
               [multi_group(pattern),
                option(regex/1),
                option(range/1),
                meta_option(pattern/1),
                default([range(32..126)])],
               [meta_option(generator/1),
                default(generator(randgen:fd_random(lcq, gnu)))],
               [option(seed/1), default(seed(-1))]
              ]).

:- initialization setup_options.




