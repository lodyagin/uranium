%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(randgen,
          [fd_random/4,      % :Generator, +Rand_State0,
                             % -Rand_State, -X
           fd_random/5,      % +Opts, :Generator, +Rand_State0,
                             % -Rand_State, -X
           prepare_random_state/3, % :Gen, +Rand_State0,
                                   % -Rand_State
           random_generator/4,
           random_integer/3, % +State, +End, -Value
           random_member/5,  % -X, +List, :Generator,
                             % +Rand_State0, -Rand_State
           random_member/6,  % -X, +List, +Det, :Generator,
                             % +Rand_State0, -Rand_State
           random_options/7, % +Options0, -Options, -Det,
                             % -Generator, -Rand_State0,
                             % -Rand_State, -Phase_Match
           random_select/6,  % -X, +List, -Rest, :Generator,
                             % +Rand_State0, -Rand_State
           % PRNG implementations
           lcq_gnu/2,
           lcq_knuth/2,
           pcg32_1/2,
           test_sequence1/2
          ]).

/** <module>  clpfd-compatible random number generators
*/

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(v)).
:- use_module(u(logging)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_option)).
:- use_module(u(ur_subarrays)).

:- multifile random_generator/4.
:- multifile prepare_random_state/3.

:- meta_predicate fd_random(:, +, -, -).
:- meta_predicate fd_random(:, :, +, -, -).

%% fd_random(:Generator, +Rand_State0, -Rand_State, -X) is nondet.
%
% Random distribution over possible values of the finite domain
% variable X. Randomly choose another random value on backtracing.
%
% @param LogOpts logging options
% @param Generator predicate with arity 2: Generator(Rand_State, Rand_State0)
% @param Rand_State0 previous rand_state
% @param Rand_State used for X calculation (to pass as Rand_State0 in the next call)
% @param X variable with finite domain attributes

fd_random(Generator, Rand_State0, Rand_State, X) :-
   fd_random_cmn([], Generator, Rand_State0, Rand_State, X).
fd_random(LogOpts, Generator, Rand_State0, Rand_State, X) :-
   fd_random_cmn(LogOpts, Generator, Rand_State0, Rand_State, X).

fd_random_cmn(_, Generator, Rand_State0, Rand_State, X) :-
   prepare_random_state(Generator, Rand_State0, Rand_State1),
   findall(X, indomain(X), All_Possible_Values), !,
   random_select(X, All_Possible_Values, _, Generator,
                 Rand_State1, Rand_State).


:- meta_predicate random_member(-, +, +, :, +, -).

%% random_member(-X, +List, +Det, :Generator, +Rand_State0,
%                -Rand_State).
%
% X is a random member of List.
% @param Det nondet or semidet. It selects random_member/5 or
%   random_select/6 internally depending on this parameter.
%
random_member(X, List0, Det, Generator, Rand_State0, Rand_State) :-
   Ctx = context(random_member/6, _),
   list_to_subarray(List0, List, Ctx),
   sa_length(List, N),
   N > 0,
   (  Det == nondet
   -> random_select_int(X, N, List, _, Generator, Rand_State0,
                        Rand_State)
   ;  random_member_int(X, N, List, Generator, Rand_State0,
                        Rand_State)
   ).

:- meta_predicate random_member(-, +, :, +, -).

%% random_member(-X, +List, :Generator, +Rand_State0, -Rand_State)
%%              is semidet.
%
% X is a random member of List. It is like random_member/2 but uses
% the defined Generator and Rand_State.
%
random_member(X, List0, Generator, Rand_State0, Rand_State) :-
   Ctx = context(random_member/5, _),
   list_to_subarray(List0, List, Ctx),
   sa_length(List, N),
   random_member_int(X, N, List, Generator, Rand_State0,
                     Rand_State).

random_member_int(X, 1, List, _, Rand_State, Rand_State) :- !,
   sa_nth1(1, List, X).
random_member_int(X, N, List, Generator, Rand_State0, Rand_State)
:-
   N > 0,
   call(Generator, Rand_State0, Rand_State),
   random_integer(Rand_State, N, Idx),
   sa_nth0(Idx, List, X).


:- meta_predicate random_options(+, -, -, -, -, -, -).

%% random_options(+Options0, -Options, -Det, -Generator,
%%                -Rand_State0, -Rand_State, ?Phase_Match) is det.
%
% Extracts common random options. It combines Options0 and Options0 /
% global_options / rand_options values. Options0 (local) values
% overwrites the globals.
% @param Options0 is an options object
% @param Options is an options object corresponding to Options0 but if
%   Options0/global_options/rand_options contains rand_state/1,2 Options
%   will contain the rewritten rand_state value:
%     rand_state(Rand_State0) -> rand_state(Rand_State),
%     rand_state(Rand_State0, Rand_State)
%         -> rand_state(Rand_State, Rand_State1)
% @param Phase_Match bound to {phase_match,
%   phase_mismatch, true, false} or is unbound. phase_match means
%   that phase in global options matches that one in local
%   options. It is unbound if phase is not specified
%   either in global or in local options.
%
random_options(Options0, Options, Det, Generator,
               Rand_State0, Rand_State, Phase_Match
              ) :-
   obj_rewrite(Options0, weak,
               [generator, rand_state, det, global_options, phase],
               [Generator_Opt0, Rand_State_Opt0, Det0, GO0, Req_Phase],
               [Generator_Opt0, Rand_State_Opt_LocalOut, Det0, GO, Req_Phase],
               % NB all local opts values are repeated except
               % Rand_State
               Options
               ),
   % Get the global random options
   (  nonvar(GO0) -> GO1 = GO0
   ;  obj_construct(global_options_v, [], [], GO1)
   ),
   obj_rewrite(GO1, [rand_options], [RO1], [RO_GlobOut], GO),
   ( nonvar(RO1) -> RO2 = RO1 ; RO2 = [] ),
   options_to_object(random_options, RO2, RO3), % the opt def. must
                                                % contain no defaults
                                                % The overwrite rule:
                                                % locals have more
                                                % priority
   obj_rewrite(RO3,
               [generator, rand_state, det, phase],
               [Generator_Opt3, Rand_State_Opt3, Det3, Cur_Phase],
               [Generator_Opt3, Rand_State_Opt_GlobOut, Det3, Cur_Phase],
               % NB rewrite only rand_state
               RO_GlobOut),
   % phase match checker
   (  ( var(Req_Phase) ; var(Cur_Phase) )
   -> true
   ;  Req_Phase = phase(Req_Phase_Val),
      Cur_Phase = phase(Cur_Phase_Val),
      must_be(integer, Req_Phase_Val),
      must_be(integer, Cur_Phase_Val),
      (  Req_Phase_Val == Cur_Phase_Val
      -> memberchk(Phase_Match, [phase_match, true])
      ;  memberchk(Phase_Match, [phase_mismatch, false])
      )
   ),
   % generator
   (  nonvar(Generator_Opt0)
   -> Generator_Opt0 = generator(Generator)
   ;  nonvar(Generator_Opt3)
   -> Generator_Opt3 = generator(Generator)
   ;  Generator = randgen:lcq_gnu % the default
   ),
   must_be(callable, Generator),
   % rand_state
   (  nonvar(Rand_State_Opt0)
   -> Rand_State_Opt = Rand_State_Opt0,
      Rand_State_Opt_Out = Rand_State_Opt_LocalOut
   ;  nonvar(Rand_State_Opt3)
   -> Rand_State_Opt = Rand_State_Opt3,
      Rand_State_Opt_Out = Rand_State_Opt_GlobOut
   ;  Rand_State_Opt = rand_state(random_seed)
   ),
   memberchk(Rand_State_Opt,
             [rand_state(Rand_State1),
              rand_state(Rand_State1, Rand_State)]),
   prepare_random_state(Generator, Rand_State1, Rand_State0),
   % det
   (  nonvar(Det0)
   -> Det = Det0
   ;  nonvar(Det3)
   -> Det = Det3
   ;  Det = semidet % the default
   ),
   % make the new options object
   functor(Rand_State_Opt, _, Rand_State_Arity),
   (  Rand_State_Arity == 1
   -> Rand_State_Opt_Out = rand_state(Rand_State)
   ;  Rand_State_Opt_Out = rand_state(Rand_State, _)
   ).

:- meta_predicate random_select(-, +, -, :, +, -).

%% random_select(-X, +List, -Rest, :Generator, +Rand_State0,
%                -Rand_State) is nondet.
%
% Randomly select an element. It selects other elements randomly on
% BT. Fails if List is an empty list.
%
random_select(X,List0, Rest, Generator, Rand_State0, Rand_State) :-
   Ctx = context(random_select/6, _),
   list_to_subarray(List0, List, Ctx),
   sa_length(List, N),
   random_select_int(X, N, List, Rest, Generator, Rand_State0,
                     Rand_State).

random_select_int(X, 1, List, [], _, Rand_State, Rand_State) :- !,
   sa_nth1(1, List, X).
random_select_int(X, N, List, Rest, Generator, Rand_State0,
                  Rand_State) :-
   N > 0,
   call(Generator, Rand_State0, Rand_State1),
   random_integer(Rand_State1, N, Idx),
   sa_nth0(Idx, List, X1, Rest1),
   (  X = X1, Rest = Rest1, Rand_State1 = Rand_State
   ;  succ(N1, N),
      random_select_int(X, N1, Rest1, Rest, Generator,
                        Rand_State1, Rand_State)
   ).


%% random_generator(?Family, ?Name, -Pred, -Max_Value)
%
% Select generator Pred by Family/Name. Pred should generate random numbers
% in the range 0 .. Max_Value

% This group is from
% http://en.wikipedia.org/wiki/Linear_congruential_generator
% NB the last number is mod - 1!
random_generator(lcq, gnu, randgen:lcq_gnu, 4294967295).
random_generator(lcq, knuth, randgen:lcq_knuth, 18446744073709551615).

% The test generator - returns sequence of numbers from 0 to 1e9-1
random_generator(test, sequence1, randgen:test_sequence1, 1000000000).

random_generator(pcg, impl1, randgen:pcg32_1, 0x100000000).

%TODO random_generator(?Spec, ?Family, ?Name, ?Pred, -Max).

lcq_gnu(X0, X) :-
  X #= (1103515245 * X0 + 12345) mod 4294967296.

lcq_knuth(X0, X) :-
  X #= (6364136223846793005 * X0 + 1442695040888963407)
  mod 18446744073709551616.

test_sequence1(X0, X) :-
  X #= (X0 + 1) mod 1000000000.

pcg32_1(pcg32_state(State0, Inc), pcg32_state(State, Inc)) :-
  State is ((State0 * 6364136223846793005) /\ 0xffffffffffffffff) + (Inc \/ 1).

%% random_integer(State, End, Value) is det.
%
% Extracts the integer value in range [0, End) from State.
%
random_integer(random_seed, _, _) :-
  throw(error(random_seed_in_evaluation, context(random_integer/3, _))).
random_integer(pcg32_state(State, _), End, Value) :- !,
  Xor_Shifted is (((State >> 18) xor State) >> 27) /\ 0xffffffff,
  Rot is State >> 59,
  Value0 is ((Xor_Shifted >> Rot) \/ (Xor_Shifted << ((-Rot) /\ 31))) /\ 0xffffffff,
  Value is Value0 mod End,
  debug(randgen, "pcg32_state(~p, _) -> ~d mod ~d -> ~d",
        [State, Value0, End, Value]).
random_integer(State, End, Value) :-
  integer(State), !,
  Value is State mod End.

:- meta_predicate prepare_random_state(:, +, -).
%% prepare_random_state(:Gen, +Rand_State0, -Rand_State) is det.
%
% Checks and prepare Rand_State for usage in random number generation
% with Gen.
%
prepare_random_state(randgen:Gen, Rand_State0, Rand_State) :-
  memberchk(Gen, [lcq_gnu, lcq_knuth, test_sequence1]), !,
  random_generator(_, _, randgen:Gen, End), !,
  random_seed(End, Rand_State0, Rand_State).
prepare_random_state(randgen:pcg32_1,
                     pcg32_state(S, I),
                     pcg32_state(S, I)) :- !.
prepare_random_state(randgen:pcg32_1,
                     pcg32_init(SI, I0),
                     pcg32_state(S, I)) :- !,
  I is (I0 << 1) \/ 1,
  pcg32_1(pcg32_state(0, I), pcg32_state(S1, I)),
  S2 is S1 + SI,
  pcg32_1(pcg32_state(S2, I), pcg32_state(S, I)).
prepare_random_state(_, Rand_State, _) :-
  throw(error(domain_error(random_state, Rand_State), _)).


%% random_seed(+End, -Seed) is det.
%
% Return random seed (varied with clock) in [0, End) range.
%
random_seed(End, Seed) :- Seed is random(End).

%% random_seed(+End, +Seed0, -Seed) is det.
%
% If Seed0 == random_seed then use random_seed/2 to calculate Seed.
% Otherwise checks that Seed0 is integern and then Seed = Seed0.
%
random_seed(End, random_seed, Seed) :- !, random_seed(End, Seed).
random_seed(End, Seed0, Seed) :- integer(Seed0), !, Seed is Seed0 mod End.
random_seed(_, Seed, _) :- throw(error(domain_error(random_state, Seed), _)).
