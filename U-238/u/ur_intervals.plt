:- begin_tests(ur_intervals).
:- use_module(u(ur_intervals)).
:- use_module(library(clpfd)).

test(dom_list1) :-
  A in 1..10, A#\=3, A#\=6, A#\=9, fd_dom(A, Dom1),
  dom_list(Dom1, List1),
  dom_list(Dom2, List1),
  assertion(List1 == [1..2, 4..5, 7..8, 10..10]),
  assertion(Dom1 == Dom2).

:- end_tests(ur_intervals).