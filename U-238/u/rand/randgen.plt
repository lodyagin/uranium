:- begin_tests(randgen).
:- use_module(randgen).
:- use_module(library(clpfd)).

test(fd_random1_gnu) :-

   X in 5..15,
   fd_random(lcq_gnu, 0, S1, X), !,
   fd_random(lcq_gnu, 0, S1, X), !,
   assertion(S1 == 12345),
   assertion(X == 8).

test(fd_random1_knuth) :-

   X in 5..15,
   fd_random(lcq_knuth, 0, S1, X), !,
   fd_random(lcq_knuth, 0, S1, X), !,
   assertion(S1 == 1442695040888963407),
   assertion(X == 12).

test(random_select1, [L == ""]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "", G, 0, _), L).

test(random_select1, [L == "a"]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "a", G, 0, _), L).

test(random_select1, [L == "BDFHJLNPRTVXZciouamyqkwesg"]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "aBcDeFgHiJkLmNoPqRsTuVwXyZ", G, 0, _), L).

:- end_tests(randgen).
