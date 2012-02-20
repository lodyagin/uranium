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

% This module defines various clpfd-compatible random number generators.

:- module(randgen,
          [random_generator/4,
           lcq_gnu/2,
           lcq_knuth/2,
           std_random/2
          ]).

:- use_module(library(clpfd)).
:- use_module(u(clpfd_adds)).

% This group is from
% http://en.wikipedia.org/wiki/Linear_congruential_generator
% NB the last number is mod - 1!
random_generator(lcq, gnu, lcq_gnu, 4294967295).
random_generator(lcq, knuth, lcq_knuth, 18446744073709551615).
random_generator(std, std, std_random, use_domain).

lcq_gnu(X0, X) :-

  X #= (1103515245 * X0 + 12345) mod 4294967296.

lcq_knuth(X0, X) :-

  X #= (6364136223846793005 * X0 + 1442695040888963407)
  mod 18446744073709551616.

% Linear distribution over X domain
std_random(Drep, X) :-

  fd_size(X, Max),
  succ(Max, Max1),
  Idx is random(Max1),
  drep_nth0(Idx, Drep, X).

