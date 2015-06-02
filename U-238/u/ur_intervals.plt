:- begin_tests(ur_intervals).
:- use_module(u(ur_intervals)).
:- use_module(library(clpfd)).

test(dom_nth0) :-
  dom1(Dom1),
  findall( El,
           ( between(0, 6, Idx),
             dom_nth0(Idx, Dom1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Dom2),
  A in Dom2,
  fd_dom(A, Dom3),
  assertion(Dom1 == Dom3).
  
test(dom_nth1) :-
  dom1(Dom1),
  findall( El,
           ( between(1, 7, Idx),
             dom_nth1(Idx, Dom1, El) ),
           List
         ),
  clpfd:list_to_drep(List, Dom2),
  A in Dom2,
  fd_dom(A, Dom3),
  assertion(Dom1 == Dom3).
  
dom1(Dom) :- 
  A in 1..10, A#\=3, A#\=6, A#\=9, fd_dom(A, Dom).
  
:- end_tests(ur_intervals).
