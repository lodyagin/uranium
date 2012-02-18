:- use_module(library(clpfd)).

randseq(L) :-

   randseq(L, S),
   Max is 2 ^ 32 - 1,
   S ins 0..Max,
   label(S).

randseq([], []) :- !.

randseq([X], [Seed]) :-

   norm(Seed, X), !.

randseq([X|L], [Seed|S]) :-

   S = [Seed1|_],
   norm(Seed, X),
   lcq(Seed, Seed1),
   randseq(L, S).

lcq(X0, X) :-

   X #= (1103515245 * X0 + 12345) mod 2 ^ 32.

% map Seed to X domain
norm(Seed, X) :-
   fd_inf(X, Min),
   fd_sup(X, Max),
   X #= Seed mod (Max - Min + 1) + Min.

   