% example
%
% pat3(X, I) :- X ins 97..122, all_different(I).
%
% :- random_string([random_generator(lcq, gnu), length(15),
%    pattern(pat3)], Codes), atom_codes(Atom, Codes).

:- module(gt_strings,
          [random_string/1,
           random_string/2
           ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).
:- use_module(u(clpfd_adds)).
:- use_module(u(rand/randgen)).
:- use_module(u(rand/randseq)).

% random_string(:Constraint, +Options, -Str)
% options
% length(N) - string of length N
% length(N1, N2) - string of length N1..N2 (linear random)

random_string(Str) :-

  random_string([], Str).

random_string(Options, Str) :-

  must_be(list, Options),

  random_string(Options,
                Options,
                default(random_generator(lcq, gnu)),
                default_to_multi([empty, length(1, 80)]),
                default(pattern(default_string_pattern)),
                Str).

random_string([O|Os], Options, Generator, Lengths, Pattern, Str) :-

  (  var(O) -> instantiation_error(O)
  ;  override(length, Lengths, O, Options, Lengths1) ->
     random_string(Os, Options, Generator, Lengths1, Pattern, Str)
  ;  override(generator, Generator, O, Options, Generator1) ->
     random_string(Os, Options, Generator1, Lengths, Pattern, Str)
  ;  override(pattern, Pattern, O, Options, Pattern1) ->
     random_string(Os, Options, Generator, Lengths, Pattern1, Str)
  ;  domain_error(random_string_option, O)
  ).

random_string([], _, Generator, Lengths, Pattern, Str) :-

  maplist(arg(1), [Generator, Lengths, Pattern], [G, L, pattern(P)]),
  random_string2(G, L, P, Str).

random_string2(Generator, Lengths, Pattern, Str) :-

  % Randomly choose the string length
  choose_length(Lengths, N),

  (  N =:= 0
  ->
     Str = []
  ;
     % Generate string equation
     length(Str, N),
     length(Indexes, N),
     call(Pattern, Str, Indexes),
     randseq(Str, Generator, Seeds, Indexes),
     
     % Randomization of the result
     Seeds = [Seed|_],
     call(Generator, _, Max_Seed),
     Random_Par is Max_Seed + 1,

     repeat, % may be try different seeds
     Seed is random(Random_Par),

     % Find the indexes
     label(Seeds),
     !,

     % Map indexes to domains
     idx_dom_list(Indexes, Str)
  ).
  
generator(random_generator(Class, Type)) :-
  must_be(atom, Class),
  must_be(atom, Type).
  
length(empty) :- !.
length(length(N)) :- !,
  must_be(integer, N).
length(length(N1, N2)) :- !,
  must_be(nonneg, N1),
  must_be(nonneg, N2).
  % TODO check N2 >= N1

pattern(pattern(Pattern)) :-
  must_be(callable, Pattern).


override(What, Prev, Value, Options, Result) :-
        call(What, Value),
        override_(Prev, Value, Options, Result).

override_(default(_), Value, _, user(Value)) :- !.
override_(user(Prev), Value, Options, _) :- !,
        (   Value == Prev ->
            domain_error(nonrepeating_options, Options)
        ;   domain_error(consistent_options, Options)
        ).
override_(default_to_multi(_), Value, _, multi([Value])) :- !.
override_(multi([]), Value, _, multi([Value])) :- !.
override_(multi([V1|T]), Value, _, multi([Value, V1|T])).

% Randomly choose length with proper distribution
choose_length(Lengths, L) :-
  
    T =.. [length|Lengths],
    functor(T, _, M),
    K is 1 + random(M),
    arg(K, T, Arg),
    choose_length2(Arg, L).

choose_length2(empty, 0) :- !.
choose_length2(length(N), N) :- !.
choose_length2(length(From, To), N) :-
  N is From + random(To - From + 1).

default_string_pattern(L, _) :-

  L ins 32 .. 126.
