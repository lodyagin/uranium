% example:
% length(L, 20), L ins 32..126, randseq(32, 126, L, S), S ins 0..4294967295, all_different(L), label(S), writeln(L).

:- use_module(library(clpfd)).

randseq(Min .. Max, L) :-

   randseq(Min, Max, L, _).

randseq(_, _, [], []) :- !.

randseq(Min, Max, [X], [Seed]) :-

   norm(Min, Max, Seed, X), !.

randseq(Min, Max, [X|L], [Seed|S]) :-

   S = [Seed1|_],
   norm(Min, Max, Seed, X),
   lcq(Seed, Seed1),
   randseq(Min, Max, L, S).

lcq(X0, X) :-

   X #= (1103515245 * X0 + 12345) mod 2 ^ 32.

% map Seed to X domain
norm(Min, Max, Seed, X) :-
   X #= Seed mod (Max - Min + 1) + Min.

   