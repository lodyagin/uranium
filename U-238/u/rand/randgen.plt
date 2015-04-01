:- begin_tests(randgen).
:- use_module(randgen).
:- use_module(library(clpfd)).

test(fd_random1_gnu) :-

   X in 5..15,
   fd_random(lcq, gnu, 0, S1, X),
   fd_random(lcq, gnu, 0, S1, X),
   assertion(S1 == 12345),
   assertion(X == 8).

test(fd_random1_knuth) :-

   X in 5..15,
   fd_random(lcq, knuth, 0, S1, X),
   fd_random(lcq, knuth, 0, S1, X),
   assertion(S1 == 1442695040888963407),
   assertion(X == 12).

:- end_tests(randgen).
