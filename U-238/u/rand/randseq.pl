:- module(randseq,
          [randseq/2,
           lcq/2]
         ).

:- use_module(library(clpfd)).

lcq(gnu, lcq(1103515245, 12345, 4294967296)).

randseq(LCQ, L) :-

   LCQ = lcq(_, _, M),
   randseq(LCQ, L, S),
   Max is M - 1,
   S ins 0..Max,
   label(S).

randseq(_, [], []) :- !.

randseq(_, [X], [Seed]) :-

   norm(Seed, X), !.

randseq(LCQ, [X|L], [Seed|S]) :-

   S = [Seed1|_],
   norm(Seed, X),
   lcq_eval(LCQ, Seed, Seed1),
   randseq(LCQ, L, S).

lcq_eval(lcq(A, C, M), X0, X) :-

   X #= (A * X0 + C) mod M.

% map Seed to X domain
norm(Seed, X) :-
   fd_inf(X, Min),
   fd_sup(X, Max),
   X #= Seed mod (Max - Min + 1) + Min.

   