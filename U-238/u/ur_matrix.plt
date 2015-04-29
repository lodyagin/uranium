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

test(unify1, A == [[0.5, 6], [16, 13]]) :-
   M = [[_/2, 3*_], [_+5, _+1]],
   unify([[1, 2], [11, 12]], M),
   mapmatrix(is, A, M).

test(unify2, A == [[0.5, 6], [16, 13]]) :-
   M = [[_/2, 3*_], [_+5, _+1]],
   unify(M, [[1, 2], [11, 12]]),
   mapmatrix(is, A, M).

test(unify3, fail) :-
   M = [[_/2, 3], [_+5, _+1]],
   unify(M, [[1, 2], [11, 12]]).

test(unify4, M1 == M2) :-
   M1 = [[1, 2], [11, 12]],
   M2 = [[_, _], [_, _]],
   unify(M1, M2).

test(unify5, M1 == M2) :-
   M2 = [[1, 2], [11, 12]],
   M1 = [[_, _], [_, _]],
   unify(M1, M2).

:- end_tests(ur_matrix).
