% example:
% lcq(gnu, LCQ), length(L, 5), L ins 1..100, randseq(LCQ, L, S), repeat, X0 is random(100), L=[X0|_], label(S), !, atom_codes(A, L).

:- module(randseq,
          [randseq/3,
           lcq/2]
         ).

:- use_module(library(clpfd)).

lcq(gnu, lcq(1103515245, 12345, 4294967296)).

randseq(LCQ, L, S) :-

   LCQ = lcq(_, _, M),
   randseq2(LCQ, L, S),
   Max is M - 1,
   S ins 0..Max.

randseq2(_, [], []) :- !.

randseq2(_, [X], [Seed]) :-

   norm(Seed, X), !.

randseq2(LCQ, [X|L], [Seed|S]) :-

   S = [Seed1|_],
   norm(Seed, X),
   lcq_eval(LCQ, Seed, Seed1),
   randseq2(LCQ, L, S).

lcq_eval(lcq(A, C, M), X0, X) :-

   X #= (A * X0 + C) mod M.

% map Seed to X domain
norm(Seed, X) :-
   fd_inf(X, Min),
   fd_sup(X, Max),
   X #= Seed mod (Max - Min + 1) + Min.

   