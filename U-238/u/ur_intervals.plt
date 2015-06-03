:- begin_tests(ur_intervals).
:- use_module(u(ur_intervals)).
:- use_module(library(clpfd)).

test(intervals_nth0) :-
  intervals1(Intervals1),
  findall( El,
           ( between(0, 6, Idx),
             intervals_nth0(Idx, Intervals1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Intervals2),
  A in Intervals2,
  fd_dom(A, Intervals3),
  assertion(Intervals1 == Intervals3).
  
test(intervals_nth1) :-
  intervals1(Intervals1),
  findall( El,
           ( between(1, 7, Idx),
             intervals_nth1(Idx, Intervals1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Intervals2),
  A in Intervals2,
  fd_dom(A, Intervals3),
  assertion(Intervals1 == Intervals3).
  
intervals1(Intervals) :- 
  A in 1..10, A#\=3, A#\=6, A#\=9, fd_dom(A, Dom).
  
:- end_tests(ur_intervals).
