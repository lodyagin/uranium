:- module(grid_embedding_v, []).

:- use_module(library(error)).
:- use_module(library(pce)).
:- use_module(u(v)).

new_class(grid_embedding_v, object_v,
          [coords]).

'grid_embedding_v?'(Obj, norm_coords, Coords) :-
   obj_field(Obj, coords, Coords0),
   normalize(Coords0, Coords).

normalize(C, C). % TODO

