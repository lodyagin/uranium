:- use_module(library(clpfd)).

randseq(0, _, []).

randseq(1, Seed, [Seed]).

randseq(N, Seed, L) :-
   N > 1,
   N1 is N - 1,
   randseq(N1, Seed, L0),
   L0 = [X0|_],
   lcq(X0, X),
   L = [X|L0],
   all_different(L).

lcq(X0, X) :-

   X #= (1103515245 * X0 + 12345) mod 32.
   