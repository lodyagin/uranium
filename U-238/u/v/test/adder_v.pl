:- module(adder_v, []).

:- use_module(u(v)).

% Class for evaluation a + b = c and z + y = x
% (different fields order)

new_class(adder_v, object_v, [a, z, 'c#', 'x#']).

new_class(vector_adder_v, adder_v, []).

'adder_v?'(Obj, b, B) :-
   obj_unify(Obj, [a, 'c#'], [A, C]),
   C is A + B.

'adder_v?'(Obj, y, Y) :-
   obj_unify(Obj, [z, 'x#'], [Z, X]),
   X is Z + Y.

'adder_v?'(Obj, c, C) :-
   obj_field(Obj, 'c#', C).

'adder_v?'(Obj, x, X) :-
   obj_field(Obj, 'x#', X).

'vector_adder_v?'(Obj, b, B) :-
   obj_unify(Obj, [a, 'c#'], [A, C]),
   maplist(plus, A, B, C).




   