:- use_module(library(clpfd)).

randseq(L) :-

   randseq(L, _).

randseq([], []) :- !.

randseq([Seed], [Seed]) :- !.

randseq([X|L], [Seed|S]) :-

   norm(Seed, X),
   lcq(Seed, Seed1),
   randseq(Seed1, L, S).

lcq(X0, X) :-

   X #= (1103515245 * X0 + 12345) mod 2 ^ 32.

% map Seed to X domain
norm(Seed, X) :-
   fd_inf(X, Min),
   fd_sup(X, Max),
   X #= Seed mod (Max - Min + 1) + Min.

   