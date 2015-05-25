:- begin_tests(randgen, [setup(setup_options)]).

:- use_module(randgen).
:- use_module(library(clpfd)).
:- use_module(u(v)).

% http://codegolf.stackexchange.com/questions/42343/implement-a-pcg
test(pcg32_1test1) :-
   prepare_random_state(randgen:pcg32_1, pcg32_init(42, 52), State1),
   random_integer(State1, 0x100000000, Value1),
   assertion(Value1 == 2380307335),
   pcg32_1(State1, State2),
   random_integer(State2, 0x100000000, Value2),
   assertion(Value2 == 948027835).

test(fd_random1_gnu) :-
   X in 5..15,
   fd_random(randgen:lcq_gnu, 0, S1, X), !,
   fd_random(randgen:lcq_gnu, 0, S1, X), !,
   assertion(S1 == 12345),
   assertion(X == 8).

test(fd_random1_knuth) :-
   X in 5..15,
   fd_random(randgen:lcq_knuth, 0, S1, X), !,
   fd_random(randgen:lcq_knuth, 0, S1, X), !,
   assertion(S1 == 1442695040888963407),
   assertion(X == 12).

test(fd_random1_pcg32_1) :-
   X1 in 5..15,
   X2 in 5..15,
   fd_random(randgen:pcg32_1, pcg32_init(42, 52), S1, X1), !,
   fd_random(randgen:pcg32_1, S1, S2, X2), !,
   fd_random(randgen:pcg32_1, pcg32_init(42, 52), S1, X1), !,
   fd_random(randgen:pcg32_1, S1, S2, X2), !,
   assertion(X1 == 12),
   assertion(X2 == 10).

test(fd_random2, [L == "bdfhjlnprtvxzciouamyqkwesg"]) :-
   random_generator(test, sequence1, G, _),
   X in 0'a..0'z,
   findall(X, fd_random(G, 0, _, X), L0),
   string_codes(L, L0).

test(random_member1, [fail]) :-
   random_generator(test, sequence1, G, _),
   random_member(_, [], G, 0, _).

test(random_member2) :-
   random_generator(test, sequence1, G, _),
   random_member(X, [0'a], %'
                 G, 0, S),
   assertion(S == 0), %NB seed is unchanged
   assertion(X == 0'a). %'

test(random_member3) :-
   random_generator(test, sequence1, G, _),
   string_codes("aBcDeFgHiJkLmNoPqRsTuVwXyZ", Cs),
   random_member(X, Cs, G, 0, S),
   assertion(S == 1),
   assertion(X == 0'B). %'

test(random_member1_det1, [fail]) :-
   random_generator(test, sequence1, G, _),
   random_member(_, [], semidet, G, 0, _).

test(random_member1_det2, [fail]) :-
   random_generator(test, sequence1, G, _),
   random_member(_, [], nondet, G, 0, _).

test(random_member2_det) :-
   random_generator(test, sequence1, G, _),
   random_member(X1, [a], semidet, G, 0, S1),
   assertion(S1 == 0),
   assertion(X1 == a),
   findall(p(X, S), random_member(X, [a], nondet, G, 0, S), L),
   assertion(L == [p(a, 0)]).

test(random_member3_det) :-
   random_generator(test, sequence1, G, _),
   string_codes("aBcDeFgHiJkLmNoPqRsTuVwXyZ", Cs),
   random_member(X1, Cs, G, 0, S1),
   assertion(S1 == 1),
   assertion(X1 == 0'B), %'
   findall(X,
           random_member(X, Cs,nondet,G, 0, _),
           L),
   string_codes("BDFHJLNPRTVXZciouamyqkwesg", ExpCs),
   assertion(L == ExpCs).

test(random_select1, [L == []]) :-
   random_generator(test, sequence1, G, _),
   findall(X, random_select(X, [], _, G, 0, _), L).

test(random_select2, [L == [p(0'a, [])]]) :- %'
   random_generator(test, sequence1, G, _),
   findall(p(X, R), random_select(X, [0'a], %'
                                  R, G, 0, _), L).

test(random_select3, [L == ExpCs]) :-
   random_generator(test, sequence1, G, _),
   string_codes("aBcDeFgHiJkLmNoPqRsTuVwXyZ", Cs),
   findall(X, random_select(X, Cs, _, G, 0, _), L),
   string_codes("BDFHJLNPRTVXZciouamyqkwesg", ExpCs).

test(random_options1_local_only) :-
    options_object(test_pred1, [rand_state(5, Seed2), nondet,
                                generator(randgen:test_sequence1), phase(4)], O1),
    random_options(O1, O2, Det, Gen, Seed0, Seed, Phase_Match),
    assertion(Det == nondet),
    assertion(Gen == randgen:test_sequence1),
    assertion(Seed0 == 5),
    assertion(Seed2 == Seed),
    O1 / [det, generator, rand_state] ^= [O1Det, O1Gen, O1Seed],
    O2 / [det, generator, rand_state] ^= [O2Det, O2Gen,
                                    rand_state(O2SeedVal1, O2SeedVal2)],
    assertion([O1Det, O1Gen, O1Seed] == [Det,generator(Gen),rand_state(5, Seed)]),
    assertion([O1Det, O1Gen] == [O2Det, O2Gen]),
    assertion(O2SeedVal1 == 5),
    assertion(Seed2 == O2SeedVal2),
    assertion(var(Phase_Match)).

test(random_options1_global_only) :-
    options_object(test_pred1, [], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[rand_state(5, Seed2), nondet, generator(randgen:test_sequence1),
                    phase(3)]],
                  GlobalOpts),
    random_options(O1, O2, Det, Gen, Seed0, Seed, Phase_Match),
    assertion(Det == nondet),
    assertion(Gen == randgen:test_sequence1),
    assertion(Seed0 == 5),
    assertion(Seed2 == Seed),
    O1 / [det, generator, rand_state] ^= [O1Det, O1Gen, O1Seed],
    O2 / [det, generator, rand_state] ^= [O2Det, O2Gen, O2Seed],
    O2 / global_options / rand_options / [det, generator, rand_state] ^= [GODet, GOGen, GOSeed],
    assertion([O1Det, O1Gen, O1Seed] =@= [_,_,_]),
    GOSeed = rand_state(SV1, SV2),
    assertion([GODet, GOGen, SV1] == [Det,generator(Gen), Seed]),
    assertion(SV2 =@= _),
    assertion([O1Det, O1Gen, O2Seed] =@= [O2Det, O2Gen, O2Seed]),
    assertion(var(Phase_Match)).

test(random_options1_local_overwrite) :-
    options_object(test_pred1,
                   [rand_state(5, Seed1), generator(randgen:test_sequence1), semidet],
                   O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[rand_state(15, Seed2), nondet, phase(3)]],
                  GlobalOpts),
    random_options(O1, O2, Det, Gen, Seed0, Seed, Phase_Match),
    assertion(Det == semidet),
    assertion(Gen == randgen:test_sequence1),
    assertion(Seed0 == 5),
    assertion(Seed1 == Seed),
    assertion(Seed1 \== Seed2),
    O1 / [det, generator, rand_state] ^= [O1Det, O1Gen, O1Seed],
    O2 / [det, generator, rand_state] ^= [O2Det, O2Gen,
                                    rand_state(O2SeedVal1, O2SeedVal2)],
    O2 / global_options ^= GlobalOptsOut,
    assertion([O1Det, O1Gen, O1Seed] == [Det,generator(Gen),rand_state(5, Seed)]),
    assertion([O1Det, O1Gen] == [O2Det, O2Gen]),
    assertion(O2SeedVal1 == 5),
    assertion(O2SeedVal2 == Seed1),
    obj_rewrite(GlobalOpts, [rand_options], [RO0], [_], GlobalOptsC),
    obj_rewrite(GlobalOptsOut, [rand_options], [ROOut], [_], GlobalOptsOutC),
    assertion(GlobalOptsC =@= GlobalOptsOutC),
    options_object(randgen:random_options, randgen:RO0, RO),
    assertion(rand_state(Seed, _) =^= ROOut/rand_state),
    ROOut/rand_state ^= RO/rand_state,
    assertion(RO =@= ROOut),
    assertion(var(Phase_Match)).

test(random_options2_global_only) :-
    options_object(test_pred1, [], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[rand_state(5), nondet, generator(randgen:test_sequence1)]],
                  GlobalOpts),
    random_options(O1, O2, Det, Gen, Seed0, Seed, Phase_Match),
    assertion(Det == nondet),
    assertion(Gen == randgen:test_sequence1),
    assertion(Seed0 == 5),
    assertion(var(Seed)),
    O1 / [det, generator, rand_state] ^= [O1Det, O1Gen, O1Seed],
    O2 / [det, generator, rand_state] ^= [O2Det, O2Gen, O2Seed],
    O2 / global_options / rand_options / [det, generator, rand_state] 
            ^= [GODet, GOGen, GOSeed],
    assertion([O1Det, O1Gen, O1Seed] =@= [_,_,_]),
    GOSeed = rand_state(SV1),
    assertion([GODet, GOGen, SV1] == [Det,generator(Gen), Seed]),
    assertion([O1Det, O1Gen, O2Seed] =@= [O2Det, O2Gen, O2Seed]),
    assertion(var(Phase_Match)).

test(random_options_phase_match_match) :-
    options_object(test_pred1, [phase(1)], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[phase(1)]],
                  GlobalOpts),
    random_options(O1, _, _, _, _, _, Phase_Match),
    assertion(Phase_Match == phase_match),
    random_options(O1, _, _, _, _, _, phase_match),
    random_options(O1, _, _, _, _, _, true).

test(random_options_phase_match_mismatch) :-
    options_object(test_pred1, [phase(2)], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[phase(1)]],
                  GlobalOpts),
    random_options(O1, _, _, _, _, _, Phase_Match),
    assertion(Phase_Match == phase_mismatch),
    random_options(O1, _, _, _, _, _, phase_mismatch),
    random_options(O1, _, _, _, _, _, false).

test(random_options_phase_match_unbound1) :-
    options_object(test_pred1, [phase(2)], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[]],
                  GlobalOpts),
    random_options(O1, _, _, _, _, _, Phase_Match),
    assertion(var(Phase_Match)),
    random_options(O1, _, _, _, _, _, phase_mismatch),
    random_options(O1, _, _, _, _, _, phase_match).

test(random_options_phase_match_unbound2) :-
    options_object(test_pred1, [], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[phase(1)]],
                  GlobalOpts),
    random_options(O1, _, _, _, _, _, Phase_Match),
    assertion(var(Phase_Match)),
    random_options(O1, _, _, _, _, _, phase_mismatch),
    random_options(O1, _, _, _, _, _, phase_match).

test(random_options_phase_match_unbound3) :-
    options_object(test_pred1, [], O1),
    O1 / global_options ^= GlobalOpts,
    obj_construct(global_options_v,
                  [rand_options],
                  [[]],
                  GlobalOpts),
    random_options(O1, _, _, _, _, _, Phase_Match),
    assertion(var(Phase_Match)),
    random_options(O1, _, _, _, _, _, phase_mismatch),
    random_options(O1, _, _, _, _, _, phase_match).

setup_options :-
   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),
   class_options:setup_options,
   ur_options(test_pred1,
              [[meta_option(generator/1)],
               [group(rand_state), option(rand_state/1), option(rand_state/2)],
               [group(det), option(semidet/0), option(nondet/0)],
               [group(phase), option(phase/1)]
              ]).

test_pred1(_).

:- end_tests(randgen).
