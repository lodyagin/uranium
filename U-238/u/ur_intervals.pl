% Operating in CPL(FD) domains as intervals.
% @author Sergei Lodyagin

:- module(ur_intervals,
          [dom_list/2, % ?Dom, ?List
           dom_nth1/2  % ?Idx, +Dom, ?N
          ]).

%% dom_list(?Dom, ?List) is det.
%
% Converts between FD domain and a list of intervals [A(1)b..A(1)e, A(2)b..A(2)e, ...], 
% A(k+1)b > A(k)e + 1, A(i)e >= A(i)b.
%
dom_list(Dom, List) :-
  nonvar(Dom), !,
  dom_to_list(Dom, List, []).
dom_list(Dom, List) :-
  nonvar(List), !,
  (  List = [Head|Tail]
  -> list_to_dom(Tail, Head, Dom)
  ;  throw(error(domain_error(nonempty_list, List), context(dom_list/2, _)))
  ).
dom_list(_,_) :-
  throw(error(instantiation_error, context(dom_list/2, _))).

list_to_dom([T0], Dom0, \/(Dom0, T)) :- !,
  interval_to_number(T0, T).
list_to_dom([A0|T0], Dom0, \/(Dom, A)) :-
  interval_to_number(A0, A),
  list_to_dom(T0, Dom0, Dom).
  

dom_to_list(\/(Head, Last0), List, Tail) :- !,
  number_to_interval(Last0, Last),
  dom_to_list(Head, List, [Last|Tail]).
dom_to_list(Interval0, [Interval|Tail], Tail) :-
  number_to_interval(Interval0, Interval).

number_to_interval(A..B, A..B) :- !.
number_to_interval(A, A..A).

interval_to_number(A..A, A) :- !.
interval_to_number(A..B, A..B).

%% dom_nth1(?Idx, +Dom, ?N)
%
%dom_nth1(Idx, Dom, N) :-
  
