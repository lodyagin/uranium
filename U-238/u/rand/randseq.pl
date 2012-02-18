% example:
% lcq(gnu, LCQ), length(L, 5), xml_nmtoken(L), randseq(L, LCQ, S, IL), label(S), idx_dom_list(IL, L), atom_codes(A, L).

:- module(randseq,
          [randseq/4,
           lcq/2]
         ).

:- use_module(library(clpfd)).

lcq(gnu, lcq(1103515245, 12345, 4294967296)).

randseq(L, LCQ, S, Idxs) :-

   LCQ = lcq(_, _, M),
   randseq2(L, LCQ, S, Idxs),
   Max is M - 1,
   S ins 0..Max.

randseq2([], _, [], []) :- !.

randseq2([X], _, [Seed], [Idx]) :-

   norm(X, Seed, Idx), !.

randseq2([X|L], LCQ, [Seed|S], [Idx|IL]) :-

   S = [Seed1|_],
   norm(X, Seed, Idx),
   lcq_eval(LCQ, Seed, Seed1),
   randseq2(L, LCQ, S, IL).

lcq_eval(lcq(A, C, M), X0, X) :-

   X #= (A * X0 + C) mod M.

% norm(@X, +Seed, -Idx)
% map Seed to X domain index
norm(X, Seed, Idx) :-

  fd_size(X, Size),
  Idx #= Seed mod Size.

   