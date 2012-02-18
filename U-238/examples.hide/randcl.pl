:- use_module(library(clpfd)).

%randseq(Seed, L) :-

%   randseq(Seed, L, _).


randseq(_, []) :- !.

randseq(Seed, [Seed]) :- !.

randseq(Seed, [Seed|S]) :-

   lcq(Seed, Seed1),
   randseq(Seed1, S).

lcq(X0, X) :-

   X #= (1103515245 * X0 + 12345) mod 2 ^ 32.
   