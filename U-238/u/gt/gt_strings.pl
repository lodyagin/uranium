%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012  Sergei Lodyagin
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
	   random_string/3,
	   random_string/4,
	   range_pattern/2
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(library(option)).
:- use_module(u(clpfd_adds)).
:- use_module(u(regex/regex)).

:- predicate_options(random_string/2, 1,
                     [empty,
                      length(nonneg),
                      length(nonneg, nonneg),
                      range(nonvar),
                      regex(codes)
                     ]).

:- predicate_options(random_string/3, 2,
                     [empty,
                      length(nonneg),
                      length(nonneg, nonneg),
                      range(nonvar),
                      regex(codes)
                     ]).

random_string(Str) :-

  random_string([], Str).

random_string(Options, Str) :-

  random_string(_, Options, Str).


:- meta_predicate random_string(1, +, -).

random_string(Pattern, Options, Str) :-

  random_string(Pattern, std_random, Options, Str).


:- meta_predicate random_string(1, 1, +, -).

% options
% empty - a string of length 0
% length(N) - a string of length N
% length(N1, N2) - a string of length N1..N2 (linear random)

random_string(Module:Pattern, Generator, Options, Str) :-

  must_be(callable, Generator),
  must_be(list, Options),

  (   var(Pattern)
  ->  append(Options,
	     [range(match(Drep))], % should be after user
				   % options
	     Options1),
      Pattern1 = range_pattern(Drep) % Module is this module
  ;   Pattern1 = Pattern,
      Options1 = Options
  ),

  must_be(callable, Pattern1),

  random_string_int(Module:Pattern1,
		    Generator,
		    Options1,
		    Options1,
		    default_to_multi([empty, length(1, 80)]),
		    default(range(32..126)),
		    default(regex(_)),
		    Str).

random_string_int(Pattern, Generator, [O|Os], Options,
		  Lengths, Range, Regex, Str) :- !,

  (  var(O) -> instantiation_error(O)
  ;  override(length, Lengths, O, Options, Lengths1) ->
     random_string_int(Pattern, Generator, Os, Options,
		       Lengths1, Range, Regex, Str)
  ;  override(range, Range, O, Options, Range1) ->
     random_string_int(Pattern, Generator, Os, Options,
		       Lengths, Range1, Regex, Str)
  ;  override(regex, Regex, O, Options, Regex1) ->
     random_string_int(Pattern, Generator, Os, Options,
		       Lengths, Range, Regex1, Str)
  ;  domain_error(random_string_option, O)
  ).

random_string_int(Pattern0, Generator, [], _,
		  Lengths, _, Regex, Str) :-

  maplist(arg(1), [Lengths, Regex], [L, regex(Reg)]),

  (   nonvar(Reg)
  ->  regex_pattern(Reg, Pattern)  % ignore Pattern0
  ;   Pattern = Pattern0 ),

  random_string_int(Pattern, Generator, L, Codes),
  (   nonvar(Str), Str = atom(Atom)
  ->  atom_codes(Atom, Codes)
  ;   Str = Codes
  ).

random_string_int(Pattern, Generator, Lengths, Str) :-

  % Randomly choose the string length
  choose_length(Lengths, N),

  (  N =:= 0
  ->
     Str = []
  ;
     % Generate string equation
     length(Str, N),
     call(Pattern, Str),

     % Selection
     randsel(Generator, Str)
  ).


% TODO make recursive with a seed through-passing
randsel(_, []) :- !.

randsel(Rand, [X|L]) :-

  fd_size(X, Size),
  Size > 0,

  repeat,
  call(Rand, X),
  !,
  randsel(Rand, L).


length(empty) :- !.
length(length(N)) :- !,
  must_be(integer, N).
length(length(N1, N2)) :- !,
  must_be(nonneg, N1),
  must_be(nonneg, N2).
  % TODO check N2 >= N1

range(range(_)) :- !.

regex(regex(Codes)) :- !,
	must_be(codes, Codes).

override(What, Prev, Value, Options, Result) :-
        call(What, Value),
        override_(Prev, Value, Options, Result).

override_(Stu, Prev_Stu, _, Stu) :-
	functor(Stu, _, 1),      %e.g. default(opt(Val))
	functor(Prev_Stu, _, 1), %e.g. opt(match(Val))
	arg(1, Prev_Stu, match(Val)),
	!,
	arg(1, Stu, Opt),
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


% Randomly choose length with proper distribution
choose_length(Lengths, L) :-

    length(Lengths, LLen),
    (  LLen > 1
    -> T =.. [length|Lengths],
       functor(T, _, M),
       K is 1 + random(M),
       arg(K, T, Arg)
    ;  Lengths = [Arg]
    ),
    choose_length2(Arg, L).

choose_length2(empty, 0) :- !.
choose_length2(length(N), N) :- !.
choose_length2(length(From, To), N) :-
  N is From + random(To - From + 1).


range_pattern(Drep, Str) :-

  Str ins Drep.


% Linear distribution over X domain
std_random(X) :-

  fd_dom(X, Drep),
  fd_size(X, Max),
  succ(Max, Max1),
  Idx is random(Max1),
  drep_nth0(Idx, Drep, X).

% regex -> NFA

regex_pattern(Regex, regex_pat(Nodes, Arcs)) :-

   regex_dfa(Regex, dfa(States0, Arcs, Initial, Final)),
   !,
   sort(States0, States),
   length(States, NStates),
   numlist(1, NStates, States),
   findall(sink(X), member(X, Final), L2),
   Nodes = [source(Initial)|L2].

regex_pat(Nodes, Arcs, Str) :-
   automaton(Str, Nodes, Arcs).








