%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
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

:- module(randgen,
          [fd_random/4,         % :Generator, +Seed0, -Seed, -X
           %fd_random/5,         % +GenFamily, +GenName, +Seed0, -Seed, -X
           lcq_gnu/2,
           lcq_knuth/2,
           random_generator/4,
           random_select/5      % -X, +List, :Generator, +Seed0, -Seed
          ]).

/** <module>  clpfd-compatible random number generators
*/

:- use_module(library(error)).
:- use_module(library(clpfd)).

:- multifile random_generator/4.

:- meta_predicate fd_random(:, +, -, -).

%% fd_random(:Generator, +Seed0, -Seed, -X) is nondet,
%
% Random distribution over possible values of the finite domain
% variable X. Randomly choose another random value on backtracing.
%
% @param Generator predicate with arity 2: Generator(Seed, Seed0)
% @param Seed0 previous seed
% @param Seed used for X calculation (to pass as Seed0 in the next call)
% @param X variable with finite domain attributes

fd_random(Generator, Seed0, Seed, X) :-
   must_be(integer, Seed0),
   fd_size(X, N_),
   debug(rand, 'labelling the domain of ~d elements', [N_]),
   findall(X, label([X]), All_Possible_Values), !,
   random_select(X, All_Possible_Values, Generator, Seed0, Seed).


%% fd_random(+Family, +Name, +Seed0, -Seed, -X) is nondet,
%
% Random distribution over possible values of the finite domain
% variable X. Randomly choose another random value on backtracing.
%
% @param Family random generator family
% @param Name random generator name
% @param Seed0 previous seed
% @param Seed used for X calculation (to pass as Seed0 in the next call)
% @param X variable with finite domain attributes

% fd_random(Family, Name, Seed0, Seed, X) :-
%    Ctx = context(fd_random/5, _),
%    must_be(atom, Family),
%    must_be(atom, Name),
%    (  random_generator(Family, Name, Generator, _) -> true
%    ;  throw(error(unknown_random_generator(Family, Name), Ctx))
%    ),
%    fd_random(Generator, Seed0, Seed, X).


:- meta_predicate random_select(-, +, :, +, -).

%% random_select(-X, +List, :Generator, +Seed0, -Seed) is nondet.
%
% Randomly select an element. It selects other elements randomly on
% BT. Fails if List is the empty list.
%
random_select(X, List, Generator, Seed0, Seed) :-
   length(List, N),
   N > 0,
   random_select_int(X, N, List, _, Generator, Seed0, Seed).

random_select_int(X, 1, [X], [], _, Seed, Seed) :- !.
random_select_int(X, N, List, Rest, Generator, Seed0, Seed) :-
   call(Generator, Seed0, Seed1),
   Idx is Seed1 mod N,
   nth0(Idx, List, X1, Rest1),
   (  X = X1, Rest = Rest1, Seed1 = Seed
   ;  succ(N1, N),
      random_select_int(X, N1, Rest1, Rest, Generator, Seed1, Seed)
   ).


%% random_generator(?Family, ?Name, -Pred, -Max_Value)
%
% Select generator Pred by Family/Name. Pred should generate random numbers
% in the range 0 .. Max_Value

% This group is from
% http://en.wikipedia.org/wiki/Linear_congruential_generator
% NB the last number is mod - 1!
random_generator(lcq, gnu, randgen:lcq_gnu, 4294967295).
random_generator(lcq, knuth, randgen:lcq_knuth, 18446744073709551615).

% The test generator - returns sequence of numbers from 0 to 1e9-1
random_generator(test, sequence1, randgen:test_sequence1, 1000000000).

lcq_gnu(X0, X) :-
  X #= (1103515245 * X0 + 12345) mod 4294967296.

lcq_knuth(X0, X) :-
  X #= (6364136223846793005 * X0 + 1442695040888963407)
  mod 18446744073709551616.

test_sequence1(X0, X) :-
  X #= (X0 + 1) mod 1000000000.
