% Operating in CPL(FD) domains as intervals.
% @author Sergei Lodyagin

:- module(ur_intervals,
          [dom_member/2, % ?El, +Dom
           dom_nth0/3, % ?Idx, +Dom, ?N
           dom_nth1/3  % ?Idx, +Dom, ?N
          ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).

%% dom_member(?El, +Dom) is nondet.
%
dom_member(El, Dom) :-
  Ctx = context(dom_member/2, _),
  any_to_intervals(Dom, Intervals, Ctx),
  dom_member_int(El, Intervals).

dom_member_int(Y, [n(From)-n(To)|T]) :-
  (  between(From, To, Y)
  ;  dom_member_int(Y, T)
  ).

%% dom_nth0(?Idx, +Dom, ?N)
%
dom_nth0(Idx, Dom, N) :-
  Ctx = context(dom_nth0/3, _),
  dom_nth0_cmn(Idx, Dom, N, Ctx).

%% dom_nth1(?Idx, +Dom, ?N)
%
dom_nth1(Idx, Dom, N) :-
  Ctx = context(dom_nth0/3, _),
  Idx0 #= Idx - 1,
  dom_nth0_cmn(Idx0, Dom, N, Ctx).

dom_nth0_cmn(Idx, _, _, Ctx) :-
  var(Idx), !,
  throw(error(not_implemented, Ctx)).
dom_nth0_cmn(Idx, Dom0, N, Ctx) :-
  must_be(nonneg, Idx),
  any_to_intervals(Dom0, Intervals, Ctx),
  dom_nth0_int(Idx, Intervals, N).
     
dom_nth0_int(_, [], _) :- !, fail.
dom_nth0_int(K, [n(N)-n(M)|T], El) :-
  L is M - N + 1,
  (  K < L
  -> El is N + K
  ;  K1 is K - L,
     dom_nth0_int(K1, T, El)
  ).

any_to_intervals(Any, Intervals, Ctx) :- 
  (  var(Any)
  -> (  fd_var(Any)
     -> clpfd:fd_get(Any, Dom, _),
        clpfd:domain_intervals(Dom, Intervals)
     ;  throw(error(instantiation_error, Ctx))
     )
  ;  (  Any = [_|_], Any == [] )
  -> Intervals = Any
  ;  clpfd:is_drep(Any)
  -> phrase(clpfd:drep_to_intervals(Any), Intervals)
  ;  clpfd:domain_intervals(Any, Intervals)
  -> true  
  ;  domain_error(clpfd_domain, Any)
  ).
    
