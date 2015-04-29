:- module(ur_matrix,
          [unify/2,    % +M1, +M2
           propagate/3   % +Vector, +Multiply, -Matrix
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
   propagate_cmn(Vector, Multiply, Matrix).

propagate_cmn([], _, []) :- !.
propagate_cmn(Vector, Multiply, Matrix) :-
   Vector = [[_] | _], !,
   transpose(Vector, [Vector1]),
   propagate_vertically(Vector1, Multiply, [], Matrix1),
   transpose(Matrix1, Matrix).
propagate_cmn(Vector, Multiply, Matrix) :-
   propagate_vertically(Vector, Multiply, [], Matrix).

propagate_vertically(_, 0, M, M) :- !.
propagate_vertically(V, K, M0, M) :-
   succ(K1, K),
   propagate_vertically(V, K1, [V|M0], M).



%% unify(+M1, +M2) is det.
%
% Unify cells of M1 with the first free variables of M2. If M2 have no 
% free variables but M1 has do it in other direction.
%
