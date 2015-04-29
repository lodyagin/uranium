:- module(ur_matrix,
          [mapmatrix/3,  % :Pred, ?M1, ?M2
           propagate/3,  % +Vector, +Multiply, -Matrix
           propagate/4,  % :Copy, +Vector, +Multiply, -Matrix
           unify/2       % ?M1, ?M2
          ]).

:- use_module(library(error)).
:- use_module(library(clpfd)). % for transpose
:- use_module(u(internal/check_arg)).

%% propagate(+Vector, +Multiply, -Matrix) is det.
%
% Matrix is Vector repited Multiply time.
% If Vector is in form [[A], [B], ...] (a vertical one) propagate it
% horisontally. Propagate it vertically otherwise.
%
propagate(Vector, Multiply, Matrix) :-
   Ctx = context(propagate/3, _),
   check_inst(Vector, Ctx),
   must_be(nonneg, Multiply),
   propagate_cmn((=), Vector, Multiply, Matrix).

:- meta_predicate propagate(2, +, +, -).

%% propagate(:Copy, +Vector, +Multiply, -Matrix) is det.
%
% It is like propagate/3 but uses Copy for make copy Vector-s.
%
propagate(Copy, Vector, Multiply, Matrix) :-
   Ctx = context(propagate/4, _),
   check_inst(Vector, Ctx),
   must_be(nonneg, Multiply),
   must_be(callable, Copy),
   propagate_cmn(Copy, Vector, Multiply, Matrix).
   

propagate_cmn(_, [], _, []) :- !.
propagate_cmn(Copy, Vector, Multiply, Matrix) :-
   Vector = [[_] | _], !,
   transpose(Vector, [Vector1]),
   propagate_vertically(Copy, Vector1, Multiply, [], Matrix1),
   transpose(Matrix1, Matrix).
propagate_cmn(Copy, Vector, Multiply, Matrix) :-
   propagate_vertically(Copy, Vector, Multiply, [], Matrix).

propagate_vertically(_, _, 0, M, M) :- !.
propagate_vertically(Copy, V, K, M0, M) :-
   succ(K1, K),
   call(Copy, V, V0),
   propagate_vertically(Copy, V, K1, [V0|M0], M).


:- meta_predicate mapmatrix(2, ?, ?).

%% mapmatrix(:Pred, ?M1, ?M2) is det.
%
% The same as maplist/3 but for matrix.
%
mapmatrix(_, [], []) :- !.
mapmatrix(Pred, [V1|T1], [V2|T2]) :-
   maplist(Pred, V1, V2),
   mapmatrix(Pred, T1, T2).

%% unify(?M1, ?M2) is semidet.
%
% Unify cells of M2 with the first free variables of M1. If M1 have no 
% free variables but M2 has do it in other direction. If both have not
% than fails.
%
% If M1 or M2 is just a matrix of free variables 
% it works exactly as mapmatrix((=), M1, M2).
%
%unify(M1, M2) :-
%   Ctx = context(unify/2, _),
%   term_variables(M1, 
