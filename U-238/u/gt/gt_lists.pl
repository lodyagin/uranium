:- module(gt_lists,
          [random_sublist/4  % +List, +Options0, -Options, -Sublist
          ]).

:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(u(rand/randgen)).
:- use_module(u(ur_terms)).
:- use_module(u(ur_option)).
:- use_module(u(v)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_subarrays)).

:- meta_predicate random_sublist(+, :, -, -).

%% random_sublist(+List, +Options0, -Options, -Sublist) is nondet.
%
% Randomly selects random number of elements from List as Sublist.
% The order is arbitrary, treat a list as a set, if nondet is given
% never return the same set of elements on BT.
%
random_sublist(List, OM:Options0, Options, Sublist) :-
   Ctx = context(random_sublist/4, _),
   (  nonvar(Options), Options = OM:Options1
   -> true
   ;  Options = Options1
   ),
   random_sublist_cmn(List, OM:Options0, Options1, Sublist,
                      Sublist, Ctx).

random_sublist_cmn(List0, OM:Options0, Options, Sublist, Sublist,
                   Ctx)
:-
   (  ground(Sublist)
   -> Options0 = Options
   ;
      options_to_object(random_sublist, OM:Options0, Options1),
      (  random_options(Options1, Options, Det, Generator,
                        Seed0, Seed, phase_match)
      -> list_to_subarray(List0, List, Ctx),
         sa_length(List, FullLength),
         obj_field(Options, length, Lengths),
         random_member(length(LengthDom0), Lengths,
                       Det, Generator, Seed0, Seed1),
         replace_subterms([max_subst(FullLength), calculate],
                          LengthDom0, LengthDom),
         N in LengthDom,
         fd_random(Generator, Seed1, Seed2, N),
         random_sublist_int(List, N, Generator, Seed2, Seed,
                            Sublist),
         (  Det == semidet -> ! ; true )
      ;  Options1 = Options
      )
   ).

random_sublist_int(_, 0, _, Seed, Seed, []) :- !.
random_sublist_int(List, N, Generator, Seed0, Seed, [X|T]) :-
   succ(N1, N),
   random_select(X, List, Rest, Generator, Seed0, Seed1),
   random_sublist_int(Rest, N1, Generator, Seed1, Seed, T).


max_subst(Max, max, Max).

calculate(Expr, Result) :-
    catch(Result is Expr, _, Result = Expr).

