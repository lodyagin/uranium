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

%  -*-coding: mule-utf-8-unix; fill-column: 58-*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Description      : Fixed Arithmetic
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Mar 30 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(fixed,
        [write_fixed/2 , 
         write_fixed/3 ,
         fixed_from_string/2, % deprecated
         fixed_from_term/2
    ]).

:- use_module('logging/logging').
:- use_module('lib/sl_lists').

divP(A, B, P) :-
    A mod P =:= 0,
    B is A / P.

longest_divP(A, B, P, Cnt) :-
    (divP(A, C, P), longest_divP(C, B, P, Cnt2), succ(Cnt2, Cnt), !;
     A = B, Cnt = 0).

% rational R has a finite decimal representation with less than Cnt digits after point
no_recurring_decimal(R, Pow2, Pow5, Cnt) :-
    R = _ rdiv D,
    longest_divP(D, R1, 2, Pow2),
    longest_divP(R1, 1, 5, Pow5),
    Cnt is max(Pow2, Pow5).
    

write_fixed(Fix, Max_Digits) :-
    current_output(Output),
    write_fixed(Output, Fix, Max_Digits).

% rdiv case
write_fixed(Stream, Fix, Max_Digits) :-
    functor(Fix, rdiv, 2),
    (no_recurring_decimal(Fix, _, _, Cnt), !;
     % a case with (9) , like 0.999999...
     Cnt = Max_Digits),
    format(string(Format), "~~~df", Cnt),
    format(Stream, Format, Fix).
    
%integer case
write_fixed(Stream, Fix, _) :-
    integer(Fix),
    format(Stream, "~d", Fix).

test_write_fixed(N, From, To) :-
    From < To,
    R is N rdiv From,
    (write_fixed(R, 15), write(' --- '), write(R), nl, !; true),
    succ(From, Next),
    test_write_fixed(N, Next, To).

fixed_from_string(F, S) :- 
        atom_chars(S, L),
        (
            nth1(Point_Pos, L, '.'),
            count_el(L, '.', 1), % более 1 точки не допустимо
            delete(L, '.', Clean_L),
            length(L, Length),
            atom_chars(IS, Clean_L),
            logged(term_to_atom(I, IS)),
            P_Pow is Length - Point_Pos,
            P is 10 ** P_Pow,
            F is I rdiv P, !
        ;
            % no '.' .. hmm ... may be it is an integer?
            logged(term_to_atom(F, S)),
            integer(F)
        ).

fixed_from_term(F, T) :- T \= '', fixed_from_string(F, T).


