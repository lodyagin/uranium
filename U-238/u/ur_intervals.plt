:- begin_tests(ur_intervals).
:- use_module(u(ur_intervals)).
:- use_module(library(clpfd)).

test(intervals_nth0) :-
  intervals1(Dom1, Intervals1),
  findall( El,
           ( between(0, 6, Idx),
             intervals_nth0(Idx, Intervals1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Intervals2),
  A in Intervals2,
  fd_dom(A, Intervals3),
  assertion(Dom1 == Intervals3).
  
test(intervals_nth1) :-
  intervals1(Dom1, Intervals1),
  findall( El,
           ( between(1, 7, Idx),
             intervals_nth1(Idx, Intervals1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Intervals2),
  A in Intervals2,
  fd_dom(A, Intervals3),
  assertion(Dom1 == Intervals3).

test(clpfd_domain_intervals_varvar, [error(instantiation_error,_)]) :-
   clpfd_domain_intervals(_, _).

test(clpfd_domain_intervals_empty_l, I == empty) :-
   clpfd_domain_intervals(empty, I).

test(clpfd_domain_intervals_empty_r, D == empty) :-
   clpfd_domain_intervals(D, empty).

test(clpfd_domain_intervals_from_to_finite, I == from_to(1, 10)) :-
   A in 1..10, clpfd:fd_get(A, Dom, _),
   clpfd_domain_intervals(Dom, I).

test(clpfd_domain_intervals_from_to_inf,
     error(domain_error(_, _), _)) :-
   A in inf..10, clpfd:fd_get(A, Dom, _),
   clpfd_domain_intervals(Dom, _).

test(clpfd_domain_intervals_from_to_sup,
     error(domain_error(_, _), _)) :-
   A in 1..sup, clpfd:fd_get(A, Dom, _),
   clpfd_domain_intervals(Dom, _).

test(clpfd_domain_intervals_split,
     Is == split(3, from_to(1, 2), from_to(4, 10), 9)
     ) :-
   A in 1..10, A#\=3, clpfd:fd_get(A, Dom, _),
   clpfd_domain_intervals(Dom, Is).


intervals1(Dom, Intervals) :- 
   A in 1..10, A#\=3, A#\=6, A#\=9,
   fd_dom(A, Dom),
   clpfd:fd_get(A, D, _),
   clpfd_domain_intervals(D, Intervals).

:- end_tests(ur_intervals).
