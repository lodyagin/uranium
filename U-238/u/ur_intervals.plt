:- begin_tests(ur_intervals).
:- use_module(u(ur_intervals)).
:- use_module(library(clpfd)).

test(dom_list1) :-
  dom1(Dom1),
  dom_list(Dom1, List1),
  dom_list(Dom2, List1),
  assertion(List1 == [1..2, 4..5, 7..8, 10..10]),
  assertion(Dom1 == Dom2).

test(dom_list2) :-
  A = 1, fd_dom(A, Dom1),
  dom_list(Dom1, List1),
  dom_list(Dom2, List1),
  assertion(List1 == [1..1]),
  assertion(Dom1 == Dom2).

test(dom_nth0) :-
  dom1(Dom1),
  findall( El..El,
           ( between(0, 6, Idx),
             dom_nth0(Idx, Dom1, El) ),
           List
         ),
  dom_list(Dom2, List),
  A in Dom2,
  fd_dom(A, Dom3),
  assertion(Dom1 == Dom3).
  
test(dom_nth1) :-
  dom1(Dom1),
  findall( El..El,
           ( between(1, 7, Idx),
             dom_nth1(Idx, Dom1, El) ),
           List
         ),
  dom_list(Dom2, List),
  A in Dom2,
  fd_dom(A, Dom3),
  assertion(Dom1 == Dom3).
  
dom1(Dom) :- 
  A in 1..10, A#\=3, A#\=6, A#\=9, fd_dom(A, Dom).
  
:- end_tests(ur_intervals).