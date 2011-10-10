%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(ur_math,
          [part_fact/3,
           binomial_coeff/3,
           nearest_binomial_coeff/4,
           random_list/3
           ]).

binomial_coeff(N, K, C) :-
    integer(N), N >= 0,
    integer(K), K >= 0,
    N1 is N - K + 1,
    part_fact(N1, N, A),
    part_fact(1, K, B),
    C is A / B.

% Found mimimal N: C(N, K) > C
% C1 = C(N, K)
nearest_binomial_coeff(N, K, C, C1) :-
    integer(C),
    integer(K),
    between(2, 2000, N1),
    binomial_coeff(N1, K, CC),
    CC >= C,
    C1 = CC, N = N1,
    !.

% C = A * (A+1) * (A+2) * ... * B
part_fact(A, B, C) :-
    part_fact(A, B, 1, C), !.

part_fact(A, A, U, C) :- C is A * U.

part_fact(A, B, U, C) :-
    A1 is A + 1,
    U1 is U * A,
    part_fact(A1, B, U1, C).
    
% random list - return Num random numbers 0..Ceil
random_list(Num, Ceil, List) :-
    random_list(Num, Ceil, [], List).
    
random_list(0, _, List, List) :- !.

random_list(Num, Ceil, List, Out) :-
    Num > 0,
    Num1 is Num - 1,
    R is random(Ceil),
    random_list(Num1, Ceil, [R|List], Out).
