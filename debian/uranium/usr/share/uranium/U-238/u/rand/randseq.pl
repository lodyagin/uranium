% example:
% length(L, 5), xml_nmtoken(L), randseq(L, randgen:random_generator(lcq, gnu), S, IL), label(S), idx_dom_list(IL, L), atom_codes(A, L).

:- module(randseq, [randseq/4]).

:- use_module(library(error)).
:- use_module(library(clpfd)).

randseq(L, Generator, S, Idxs) :-

   must_be(callable, Generator),
   call(Generator, LCQ, Max_Seed),
   randseq2(L, LCQ, S, Idxs),
   S ins 0..Max_Seed.

randseq2([], _, [], []) :- !.

randseq2([X], _, [Seed], [Idx]) :-

   norm(X, Seed, Idx), !.

randseq2([X|L], LCQ, [Seed|S], [Idx|IL]) :-

   S = [Seed1|_],
   norm(X, Seed, Idx),
   call(LCQ, Seed, Seed1),
   randseq2(L, LCQ, S, IL).

% norm(@X, +Seed, -Idx)
% map Seed to X domain index
norm(X, Seed, Idx) :-

  fd_size(X, Size),
  Idx #= Seed mod Size.

   