:- module(four_ops,
          [do//2,
           bin_tree_by_number/2
           ]).

do(S, S) --> [].
do(S0, S) --> do(S0, S1), step(S1, S).
   

step(state(LL0, C0, RL), state(LL, C, RL)) -->
   "l", { norm(LL0, [L1|LL1]), C = p(L1, C0), denorm(LL1, LL) }.
step(state(LL, C0, RL0), state(LL, C, RL)) -->
   "r", { norm(RL0, [R1|RL1]), C = p(C0, R1), denorm(RL1, RL) }.
step(state(LL0, C0, RL0), state(LL, C, RL)) -->
   "L",
   { norm(LL0, [L1, L2|LL1]),
     C = p(L2, L1),
     RL = [C0|RL0],
     denorm(LL1, LL)
   }.
step(state(LL0, C0, RL0), state(LL, C, RL)) -->
   "R",
   { norm(RL0, [R1, R2|RL1]),
     C = p(R1, R2),
     LL = [C0|LL0],
     denorm(RL1, RL)
   }.

norm([], [0, 0]) :-!.
norm([A], [A, 0]) :- !.
norm(L, L).

denorm([], []) :- !.
denorm([0|T1], T) :- !, denorm(T1, T).
denorm(L, L).

bin_tree_by_number(0, 0) :- !.
bin_tree_by_number(N, p(LeftT, RightT)) :-
   succ(N1, N),
   split_bits(N1, LeftN, RightN),
   bin_tree_by_number(LeftN, LeftT),
   bin_tree_by_number(RightN, RightT).

split_bits(N, Odds, Evens) :-
   to_bin_array(N, Bs),
   split_bits2(Bs, Odds, Evens).

split_bits2([], [], []) :- !.
split_bits2

to_bin_array(A, Bs) :-
   N is msb(A),
   findall(B, (between(0, N, I), B is (A >> I) /\ 1), BsR),
   reverse(BsR, Bs).

                                             