% Operating on graphs.

:- module(ur_graphs,
          [is_complete_graph/1,
           is_undirected_connected_graph/1,
           is_undirected_graph/1,
           is_undirected_path_graph/1,
           degree/3 % +Vertex, +Graph, -Degree 
          ]).

:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(ugraphs)).

is_complete_graph(G) :-
   edges(G, E), length(E, Q),
   is_undirected_graph_int(E, Q),
   vertices(G, V), length(V, P),
   Q / 2 =:= P * (P - 1) / 2.

is_undirected_connected_graph(G) :-
   edges(G, E), length(E, Q),
   vertices(G, V),
   is_undirected_connected_graph_int(G, E, Q, V).

is_undirected_connected_graph_int(G, E, Q, V) :-
   is_undirected_graph_int(E, Q),
   V = [V0|_],
   reachable(V0, G, V0R),
   sort(V, VS), sort(V0R, VS).

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

is_undirected_path_graph(G) :- 
   vertices(G, V), length(V, P), P >= 2,
   edges(G, E), length(E, Q),
   is_undirected_connected_graph_int(G, E, Q, V),
   Q / 2 =:= P - 1,
   % test with loops in graphs
   findall(D, (member(V1, V), degree(V1, G, D)), Degrees),
   msort(Degrees, [1, 1|Rest]),
   forall(member(D, Rest), D =:= 2).

%% degree(+Vertex, +Graph, -Degree) is det. 
degree(Vertex, Graph, Degree) :-
   is_undirected_graph(Graph),
   neighbours(Vertex, Graph, Ns),
   length(Ns, Degree0),
   (  memberchk(Vertex, Ns)
   -> % loops are counted twice
      succ(Degree0, Degree)
   ;  Degree = Degree0
   ).