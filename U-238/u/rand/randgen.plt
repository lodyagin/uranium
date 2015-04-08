:- begin_tests(randgen, [setup(setup_options)]).

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

test(random_member1_det1, [fail]) :-
   random_generator(test, sequence1, G, _), 
   random_member(_, "", semidet, G, 0, _).

test(random_member1_det2, [fail]) :-
   random_generator(test, sequence1, G, _), 
   random_member(_, "", nondet, G, 0, _).

test(random_member2_det) :-
   random_generator(test, sequence1, G, _), 
   random_member(X1, "a", semidet, G, 0, S1),
   assertion(S1 == 0), 
   assertion(X1 == 0'a),
   findall(p(X, S), random_member(X, "a", nondet, G, 0, S), L),
   assertion(L == [p(0'a, 0)]).

test(random_member3_det) :-
   random_generator(test, sequence1, G, _), 
   random_member(X1, "aBcDeFgHiJkLmNoPqRsTuVwXyZ", G, 0, S1),
   assertion(S1 == 1), 
   assertion(X1 == 0'B),
   findall(X, 
           random_member(X, "aBcDeFgHiJkLmNoPqRsTuVwXyZ",nondet,G, 0, _), 
           L),
   assertion(L == "BDFHJLNPRTVXZciouamyqkwesg").

test(random_select1, [L == ""]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "", _, G, 0, _), L).

test(random_select2, [L == [p(0'a, [])]]) :-
   random_generator(test, sequence1, G, _), 
   findall(p(X, R), random_select(X, "a", R, G, 0, _), L).

test(random_select3, [L == "BDFHJLNPRTVXZciouamyqkwesg"]) :-
   random_generator(test, sequence1, G, _), 
   findall(X, random_select(X, "aBcDeFgHiJkLmNoPqRsTuVwXyZ",_,G, 0, _), L).

test(random_options1) :-
    random_options(test_pred1, 
                   [seed(5, Seed1)],
                   Det, Gen, Seed0, Seed, _),
    assertion(Det == semidet),
    assertion(Gen == randgen:lcq_gnu),
    assertion(Seed0 == 5),
    assertion(Seed1 == Seed).

test(random_options2) :-
    random_options(test_pred1, 
                   [seed(6, Seed1), nondet,
                   generator(randgen:lcq_knuth)],
                   Det, Gen, Seed0, Seed, _),
    assertion(Det == nondet),
    assertion(Gen == randgen:lcq_knuth),
    assertion(Seed0 == 6),
    assertion(Seed1 == Seed).

setup_options :-
   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),
   ur_options(test_pred1,
              [[meta_option(generator/1),
                default(generator(randgen:lcq_gnu))],
               [group(seed),
                option(seed/1), option(seed/2), 
                default(seed(-1))],
               [group(det), option(semidet/0), option(nondet/0)]
              ]).

test_pred1(_).

:- end_tests(randgen).
