:- begin_tests(ur_matrix).
:- use_module(u(ur_matrix)).

test(propagate_empty1, M==[]) :-
   propagate([], 5, M).

test(propagate_empty2, M==[]) :-
   propagate([a, b, c], 0, M).

test(propagate_empty3, M==[]) :-
   propagate([[a], [b], [c]], 0, M).

test(propagate_vertically, M==[[a, b, c], [a, b, c]]) :-
   propagate([a, b, c], 2, M).

test(propagate_horisontally, M==[[a, a], [b, b], [c, c]]) :-
   propagate([[a], [b], [c]], 2, M).

test(mapmatrix, M2 == [[2,3], [12, 13]]) :-
   M1 = [[1,2], [11, 12]],
   mapmatrix(succ, M1, M2).

:- end_tests(ur_matrix).
