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

test(drep_intervals_l) :-
   Drep = 1..5 \/ 10,
   drep_intervals(Drep, I),
   I = split(S, from_to(1, 5), from_to(10, 10), 6),
   assertion(S < 10),
   assertion(S > 5).

test(drep_intervals_r, D == 1..5 \/ 10) :-
   drep_intervals(D, split(7, from_to(1, 5), from_to(10, 10), 6)).
   
test(any_to_intervals_var) :-
   Drep = 1..5 \/ 10,
   V in Drep,
   any_to_intervals(V, Intervals),
   drep_intervals(Drep2, Intervals),
   assertion(Drep == Drep2).

test(any_to_intervals_drep) :-
   Drep = 1..5 \/ 10,
   any_to_intervals(Drep, Intervals),
   drep_intervals(Drep2, Intervals),
   assertion(Drep == Drep2).

test(any_to_intervals_inst, [error(instantiation_error,_)]) :-
   any_to_intervals(_, _).

test(any_to_intervals_dom, [error(domain_error(_,_),_)]) :-
   any_to_intervals(a+b+c, _).

intervals1(Dom, Intervals) :- 
   A in 1..10, A#\=3, A#\=6, A#\=9,
   fd_dom(A, Dom),
   clpfd:fd_get(A, D, _),
   clpfd_domain_intervals(D, Intervals).

:- end_tests(ur_intervals).
