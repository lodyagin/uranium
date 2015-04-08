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

test(fd_random2, [L == "bdfhjlnprtvxzciouamyqkwesg"]) :-
   random_generator(test, sequence1, G, _), 
   X in 0'a..0'z,
   findall(X, fd_random(G, 0, _, X), L).

test(random_member1, [fail]) :-
   random_generator(test, sequence1, G, _), 
   random_member(_, "", G, 0, _).

test(random_member2) :-
   random_generator(test, sequence1, G, _), 
   random_member(X, "a", G, 0, S),
   assertion(S == 0), %NB seed is unchanged
   assertion(X == 0'a).

test(random_member3) :-
   random_generator(test, sequence1, G, _), 
   random_member(X, "aBcDeFgHiJkLmNoPqRsTuVwXyZ", G, 0, S),
   assertion(S == 1), 
   assertion(X == 0'B).

test(random_select1, [L == ""]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "", _, G, 0, _), L).

test(random_select2, [L == [p(0'a, [])]]) :-
   random_generator(test, sequence1, G, _), 
   findall(p(X, R), random_select(X, "a", R, G, 0, _), L).

test(random_select3, [L == "BDFHJLNPRTVXZciouamyqkwesg"]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "aBcDeFgHiJkLmNoPqRsTuVwXyZ",_,G, 0, _), L).

:- end_tests(randgen).
