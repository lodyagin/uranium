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
          [random_generator/4,
           lcq_gnu/2,
           lcq_knuth/2,
           fd_random/5
          ]).

/** <module>  clpfd-compatible random number generators
*/

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(clpfd_adds)).

:- multifile random_generator/4.

%% fd_random(+Family, +Name, +Seed0, -Seed, -X)
%
% Random distribution over fd_dom of X.
%
% @param Family random generator family
% @param Name random generator name
% @param Seed0 previous seed
% @param Seed used for X calculation (to pass as Seed0 in the next call)
% @param X variable with finite domain attributes

fd_random(Family, Name, Seed0, Seed, X) :-

   Ctx = context(fd_random/5, _),
   must_be(atom, Family),
   must_be(atom, Name),
   must_be(integer, Seed0),
   fd_size(X, Max),
   (  random_generator(Family, Name, Generator, _) -> true
   ;  throw(error(unknown_random_generator(Family, Name), Ctx))
   ),
   call(Generator, Seed0, Seed),
   (  Max > 1
   -> % TODO check Max_Value >> Max
      Idx is Seed mod Max,
      fd_dom(X, Drep),
      drep_nth0(Idx, Drep, X)
   ;  true
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

lcq_gnu(X0, X) :-

  X #= (1103515245 * X0 + 12345) mod 4294967296.

lcq_knuth(X0, X) :-

  X #= (6364136223846793005 * X0 + 1442695040888963407)
  mod 18446744073709551616.


