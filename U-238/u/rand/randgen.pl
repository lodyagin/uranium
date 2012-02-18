:- module(randgen,
          [random_generator/4,
           lcq_gnu/2,
           lcq_knuth/2
          ]).

:- use_module(library(clpfd)).

% This group is from
% http://en.wikipedia.org/wiki/Linear_congruential_generator
% NB the last number is mod - 1!
random_generator(lcq, gnu, lcq_gnu, 4294967295).
random_generator(lcq, knuth, lcq_knuth, 18446744073709551615).

lcq_gnu(X0, X) :-

  X #= (1103515245 * X0 + 12345) mod 4294967296.

lcq_knuth(X0, X) :-

  X #= (6364136223846793005 * X0 + 1442695040888963407)
  mod 18446744073709551616.

