:- begin_tests(ur_math).
:- use_module(ur_math).

test(bit_split1, [N==0]) :-

   bit_split([], [], N).

test(bit_split2) :-

   bit_split([], [], 4).

test(bit_split3, [N==0xA3]) :-

   bit_split([7, 4-6, 0-2], [1, 0x02, 0x03], N).

test(bit_split4, [Vals == [1, 0x02, 0x03]]) :-

   bit_split([7, 4-6, 0-2], Vals, 0xA3).

% byte_list: zeros

test(byte_list1, [L == []]) :-

   byte_list(le, 0, L).

test(byte_list2, [L == []]) :-

   byte_list(be, 0, L).

test(byte_list3, [L == [0, 0]]) :-

   L = [_, _],
   byte_list(be, 0, L).

test(byte_list4, [L == []]) :-

   L = [],
   byte_list(be, 0, L).

test(byte_list5, [L == [0]]) :-

   L = [_],
   byte_list(le, 0, L).

test(byte_list6, [N == 0]) :-

   length(L, 1000),
   maplist(=(0), L),
   byte_list(le, N, L).

% FFFF...

test(byte_list7, [N =:= 2^8000-1]) :-

   length(L, 1000),
   maplist(=(255), L),
   byte_list(le, N, L).

test(byte_list8, [L == [1, 4]]) :-

   byte_list(le, 1025, L),
   byte_list(le, N, L),
   assertion(N == 1025).

test(byte_list9, [L == [4, 1]]) :-

   byte_list(be, 1025, L),
   byte_list(be, N, L),
   assertion(N == 1025).

test(byte_list10, [L == [1, 4, 0, 0, 0]]) :-

   length(L, 5),
   byte_list(le, 1025, L),
   byte_list(le, N, L),
   assertion(N == 1025).

test(byte_list11, [L == [0, 0, 0, 4, 1]]) :-

   length(L, 5),
   byte_list(be, 1025, L),
   byte_list(be, N, L),
   assertion(N == 1025).

test(byte_list12, [L == [0, 0, 0, 0, 0, 0, 0, 0, 1]]) :-

   X is 2^64,
   length(L, 9),
   byte_list(le, X, L).

test(byte_list13,
     [error(domain_error(enough_size_list(var), L))]) :-

   X is 2^64,
   length(L, 8),
   byte_list(le, X, L).

test(byte_list14) :-

   findall(X, between(0, 255, X), L),
   byte_list(le, N, L),
   byte_list(be, N, L2),
   reverse(L, L2).

test(byte_list15) :-

   findall(X, between(0, 255, X), L1),
   byte_list(le, N, L1),
   byte_list(be, N, L2),
   byte_list(le, N2, L2),
   NN is N \/ N2,
   byte_list(le, NN, LL1),
   byte_list(be, NN, LL2),
   assertion(LL1 == LL2),
   assertion(maplist(==(255), LL1)).

test(byte_list16) :-

   findall(X, between(0, 255, X), L1),
   byte_list(le, N, L1),
   byte_list(be, N, L2),
   byte_list(le, N2, L2),
   NN is N xor N2,
   byte_list(le, NN, LL1),
   byte_list(be, NN, LL2),
   assertion(LL1 == LL2),
   assertion(maplist(==(255), LL1)).

test(byte_list17, [NN == 0]) :-

   findall(X, between(0, 255, X), L1),
   byte_list(le, N, L1),
   byte_list(be, N, L2),
   byte_list(le, N2, L2),
   NN is N /\ N2.

test(byte_list18) :-

   N is 10^100,
   byte_list(le, N, L),
   length(L12, 12),
   maplist(=(0), L12),
   append(L12, _, L).

test(byte_list19,
     [error(type_error(_, _))]
     ) :-

   byte_list(ge, 10, _).

test(byte_list20,
     [error(instantiation_error)]
     ) :-

   byte_list(le, _, _).

test(byte_list21,
     [error(instantiation_error)]
     ) :-

   byte_list(le, _, [_, _]).

test(byte_list22,
     [error(instantiation_error)]
     ) :-

   byte_list(be, _, [_]).
