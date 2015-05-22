% Operating in CPL(FD) domains as intervals.
% @author Sergei Lodyagin

:- module(ur_intervals,
          [dom_list/2, % ?Dom, ?List
           dom_nth0/3, % ?Idx, +Dom, ?N
           dom_nth1/3  % ?Idx, +Dom, ?N
          ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).

%% dom_list(?Dom, ?List) is det.
%
% Converts between FD domain and a list of intervals
% [A(1)b..A(1)e, A(2)b..A(2)e, ...], 
% A(k+1)b > A(k)e + 1, A(i)e >= A(i)b.
%
dom_list(Dom, List) :-
  nonvar(Dom), !,
  dom_to_list(Dom, List, []).
dom_list(Dom, List) :-
  nonvar(List), !,
  (  List = [Head|Tail]
  -> list_to_dom(Tail, Head, Dom)
  ;  throw(error(domain_error(nonempty_list, List),
                 context(dom_list/2, _)))
  ).
dom_list(_,_) :-
  throw(error(instantiation_error, context(dom_list/2, _))).

list_to_dom([], Dom, Dom) :- !.
list_to_dom([B0|T], Dom0, Dom) :-
  interval_to_number(B0, B),
  list_to_dom(T, \/(Dom0, B), Dom).
  
dom_to_list(\/(Head, Last0), List, Tail) :- !,
  number_to_interval(Last0, Last),
  dom_to_list(Head, List, [Last|Tail]).
dom_to_list(Interval0, [Interval|Tail], Tail) :-
  number_to_interval(Interval0, Interval).

number_to_interval(A..B, A..B) :- !.
number_to_interval(A, A..A).

interval_to_number(A..A, A) :- !.
interval_to_number(A..B, A..B).

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

dom_nth0_cmn(_, Dom, _, Ctx) :-
  var(Dom), !,
  throw(error(instantiation_error, Ctx)).
dom_nth0_cmn(Idx, _, _, Ctx) :-
  var(Idx), !,
  throw(error(not_implemented, Ctx)).
dom_nth0_cmn(Idx, Dom, N, Ctx) :-
  must_be(nonneg, Idx),
  (  dom_list(Dom, List)
  -> dom_nth0_int(Idx, List, N)
  ;  throw(error(domain_error(fd_domain, Dom), Ctx))
  ).

dom_nth0_int(_, [], _) :- !, fail.
dom_nth0_int(K, [N..M|T], El) :-
  L is M - N + 1,
  (  K < L
  -> El is N + K
  ;  K1 is K - L,
     dom_nth0_int(K1, T, El)
  ).
