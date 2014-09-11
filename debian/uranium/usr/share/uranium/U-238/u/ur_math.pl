% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
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


:- module(ur_math,
          [bit_split/3,
           byte_list/3,
           part_fact/3,
           binomial_coeff/3,
           nearest_binomial_coeff/4,
           random_list/3
           ]).

/** <module> Arithmetic and numeric functions
*/

:- use_module(library(error)).
:- use_module(u(internal/check_arg)).

%% bit_split(+Intervals, ?Values, ?Number)
%
% Intervals defines a list of intervals. One element of the list
% can be either single bit number (the lowest bit number is 0) or
% a bit range <lower>-<higher> e.g., =|2-4|=.
%
% The number of elements in the Values list is the same as in the
% Intervals.
%
% Number is composed from all Values with gaps filled with zeros
% or splitted to Values if it is ground. When composing
% conflicting intervals are ORed.

bit_split(Intervals, Values, Number) :-

   Ctx = context(bit_split/3),
   check_inst(Intervals, Ctx),
   (  var(Number)
   -> bit_split_asm(Intervals, Values, 0, Number)
   ;  bit_split_disasm(Intervals, Values, Number)
   ).


bit_split_asm([], [], N, N) :- !.

bit_split_asm([From-To|IT], [Val|VT], N0, N) :- !,

   From >= 0,
   From =< To,
   Mask is 2 ** (To - From + 1) - 1,
   Val1 is Val /\ Mask,
   Val1 =:= Val, % check Val is fit in From-To
   Val2 is Val << From,
   N1 is N0 \/ Val2,
   bit_split_asm(IT, VT, N1, N).

bit_split_asm([Bit|IT], [Val|VT], N0, N) :-

   Bit >= 0,
   Val1 is Val /\ 1,
   Val1 =:= Val, % check Val is fit in From-To
   Val2 is Val << Bit,
   N1 is N0 \/ Val2,
   bit_split_asm(IT, VT, N1, N).


bit_split_disasm([], [], _) :- !.

bit_split_disasm([From-To|IT], [Val|VT], Number) :- !,

   From >= 0,
   From =< To,
   Test is Number >> From,
   Mask is 2 ** (To - From + 1) - 1,
   Val is Test /\ Mask,
   bit_split_disasm(IT, VT, Number).

bit_split_disasm([Bit|IT], [Val|VT], Number) :-

   Bit >= 0,
   Test is Number >> Bit,
   Val is Test /\ 1,
   bit_split_disasm(IT, VT, Number).


binomial_coeff(N, K, C) :-
    integer(N), N >= 0,
    integer(K), K >= 0,
    N1 is N - K + 1,
    part_fact(N1, N, A),
    part_fact(1, K, B),
    C is A / B.

%% byte_list(+Ending, ?Number, ?Byte_List)
%
% Conversion between nonneg integers and byte lists.  If the
% Byte_List is instantiated to list a of vars of a given size it
% fills all bytes, if the byte list is var - return only non-zero
% elements. If the Byte_List contains less elements than needing
% for Number representation then the predicate fails.
%
% @param Ending *be* or *le*
%
% @error instantiation_error if both Number and Byte_List are free.

byte_list(Ending, Number, Byte_List) :-

   Ctx = context(byte_list/3, _),
   must_be(oneof([be, le]), Ending),

   (  var(Number), var(Byte_List)
   -> throw(error(instantiation_error, Ctx))
   ;  true
   ),

   (  nonvar(Number)
   -> must_be(nonneg, Number),
      (  copy_term(Byte_List, Byte_List1),
         number_to_bytes_le(Number, Byte_List1, Tail),
         (  nonvar(Tail)
         -> maplist(=(0), Tail) % the rest is zeros
         ;  Tail = []
         )
      ),
      (  Ending = le
      -> Byte_List = Byte_List1
      ;  reverse(Byte_List1, Byte_List)
      )
   ;
      (  Ending = be
      -> Byte_List1 = Byte_List
      ;  reverse(Byte_List, Byte_List1)
      ),
      bytes_to_number_le(Byte_List1, 0, Number)
   ).


number_to_bytes_le(0, L, L) :- !.

number_to_bytes_le(Number, [Byte|Tail0], Tail) :-

   Byte is Number /\ 0xFF,
   Number1 is Number >> 8,
   number_to_bytes_le(Number1, Tail0, Tail).

bytes_to_number_le([], N, N) :- !.

bytes_to_number_le([Byte|Tail], Number0, Number) :-

   must_be(between(0, 255), Byte),
   Number1 is Number0 << 8 \/ Byte,
   bytes_to_number_le(Tail, Number1, Number).


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
