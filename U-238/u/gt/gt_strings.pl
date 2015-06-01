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
          [random_string/1, % -Str
	   random_string/3, % +Options0, -Options, -Str
	   random_string/4, % +Options0, -Options, ?Str0, -Str
           randsel/5,       % +LogOpts, +List, :Gen, +Seed0, -Seed
           randselchk/5,    % +LogOpts, +List, :Gen, +Seed0, -Seed
	   range_pattern/2
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(regex/regex)).
:- use_module(u(rand/randgen)).
:- use_module(u(ur_option)).
:- use_module(u(ur_atoms)).
:- use_module(u(v)).
:- use_module(u(dict/dict)).

random_string(Str) :-
   Ctx = context(random_string/1, _),
   random_string_cmn([], _, Str, Str, Ctx).

:- meta_predicate random_string(:, -, -).

%% random_string(+Options0, -Options, -Str) is nondet.
%
% Generates a random string. It is semidet if `semidet' option passed.
%
random_string(OM:Options0, OM:Options, Str) :-
   Ctx = context(random_string/3, _),
   (  nonvar(Options), Options = OM:Options1
   -> true
   ;  Options = Options1
   ),
   random_string_cmn(OM:Options0, Options1, Str, Str, Ctx).

%% random_string(+Options0, -Options, ?Str0, -Str) is nondet.
%
% Generates a random string. If Str0 is nonvar then Str = Str0.
% It is semidet if `semidet' option passed.
%
random_string(OM:Options0, OM:Options, Str0, Str) :-
   Ctx = context(random_string/4, _),
   (  nonvar(Options), Options = OM:Options1
   -> true
   ;  Options = Options1
   ),
   random_string_cmn(OM:Options0, Options1, Str0, Str, Ctx).

random_string_cmn(OM:Options0, Options, Str, Str, _) :-
   (   nonvar(Str), ( atomic(Str); Str = [_|_] ; Str = [] )
   ->
       Options0 = Options % skip string generation
   ;
   options_to_object(random_string, OM:Options0, Options1),
   (  random_options(Options1, Options, Det, Generator, Seed1, Seed,
                     phase_match)
   ->
      obj_unify(Options1,
                [length, pattern, out_type],
                [Lengths, Patterns, OutType]),
      % NB Lengths and Patterns are always non-empty due to defaults
      random_member(Pattern1, Patterns, Det, Generator, Seed1, Seed2),

      (  Pattern1 = static(StaticStr)
      -> smth_codes(StaticStr, Codes), % Just return a static value
         Seed = Seed2
      ;
         random_member(Length, Lengths, Det, Generator, Seed2, Seed3),

         % regex/1, range/1 -> pattern/1
         context_module(That),
         (  Pattern1 = regex(Reg)
         -> regex_pattern(Reg, Pattern)
         ;  Pattern1 = range(Drep)
         -> Pattern = That:range_pattern(Drep)
         ;  Pattern1 = dict(_,_)
         -> Pattern1 = Pattern
         ;  Pattern1 = dict(Wildcard)
         -> Pattern  = dict(u('dict/zali_translit.txt'), Wildcard)
         ;  pattern(Pattern) = Pattern1,
            must_be(callable, Pattern)
         ),

	 LogOpts ^= Options / global_options / log_options,
         % Length should be properly distributed,
         % so fix the length before other things
         random_string_template(Generator, Seed3, Seed4, Length, Codes),
         (  Pattern = dict(VocabFile, Wildcard)
         -> load_random_word(VocabFile, Wildcard, Generator, Seed4, Seed,
                             Codes)
         ;  random_string_int(LogOpts, Pattern, Generator, Seed3, Seed,
                              Codes)
         )
      ),
      (  Det == semidet -> ! ; true  ),
      (   nonvar(Str), Str = atom(Atom)
      ->  atom_codes(Atom, Codes)
      ;   OutType == atom
      ->  atom_codes(Str, Codes)
      ;   OutType == string
      ->  string_codes(Str, Codes)
      ;   OutType == codes
      ->  Str = Codes
      )
   ;  Options0 = Options
   )
   ).

%!!! make nondet.
random_string_template(Generator, Seed0, Seed, Length, Templ) :-
   choose_length(Generator, Seed0, Seed, Length, N),
   length(Templ, N).

random_string_int(LogOpts, Pattern, Generator, Seed0, Seed, Str) :-
   must_be(nonvar, Str), % Str must be a list of defined length
   (  Str == []
   ->
      Seed = Seed0
   ;
      % Generate a string equation
      call(Pattern, Str),
      % Make a selection
      randsel(LogOpts, Str, Generator, Seed0, Seed)
  ).

:- meta_predicate randsel(:, +, :, +, -).

%% randsel(+LogOpts, +List, :Gen, +Seed0, -Seed) is nondet.
%
% Random labelling list members on BT.
% @see labelling/2
%
randsel(_, [], _, Seed, Seed) :- !.
randsel(LogOpts, [X|L], Gen, Seed0, Seed) :-
  fd_random(LogOpts, Gen, Seed0, Seed1, X),
  randsel(LogOpts, L, Gen, Seed1, Seed).

%% randselchk(+LogOpts, +List, :Gen, +Seed0, -Seed) is semidet.
%
% Random labelling list members. It is semidet version of randsel/4.
%
randselchk(_, [], _, Seed, Seed) :- !.
randselchk(LogOpts, [X|L], Gen, Seed0, Seed) :-
  fd_random(LogOpts, Gen, Seed0, Seed1, X), !,
  randsel(LogOpts, L, Gen, Seed1, Seed).


% Randomly choose length with proper distribution
choose_length(_, Seed, Seed, empty, 0) :- !.
choose_length(_, Seed, Seed, length(N), N) :- !.
choose_length(_, Seed, Seed, length(N, N), N) :- !.
choose_length(Gen, Seed0, Seed, length(From, To), N) :-
   N in From..To,
   fd_random(Gen, Seed0, Seed, N).

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





