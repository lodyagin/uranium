% Operating on graphs.

:- module(ur_graphs,
          [is_complete_graph/1,
           is_undirected_graph/1
          ]).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).

is_complete_graph(G) :-
   edges(G, E), length(E, Q),
   is_undirected_graph_int(E, Q),
   vertices(G, V), length(V, P),
   Q / 2 =:= P * (P - 1) / 2.

is_undirected_graph(G) :-
   edges(G, E), length(E, Q),
   is_undirected_graph_int(E, Q).

is_undirected_graph_int(E, Q) :-
   0 =:= Q mod 2, % is even
   sort(E, ES1),
   % change the direction of each edge in ES1
   findall(B-A, member(A-B, ES1), E2),
   sort(E2, ES2),
   ES1 == ES2.