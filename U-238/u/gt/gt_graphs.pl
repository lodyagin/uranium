:- module(gt_graphs,
          [random_triangulated_planar_graph/5 % +Opts0, -Opts, +K,
                                              % -Graph, -Outer)
           ]).

:- use_module(library(dialect/hprolog)).
:- use_module(library(ugraphs)).
:- use_module(u(gt/gt_numbers)).

%
% Adds a new vertex in canonical order
% to a triangulated planar graph
add_point(O0, O, V, G0, G, Outer0, Outer) :-
   length(Outer0, N), N >= 3,
   M in 2..N, % the number of covered points
   random_number(O0, O1, M),
   PMax is N - M,
   P in 0..PMax,
   random_number(O1, O, P),
   Q is P + M - 1, succ(P, P1), succ(Q1, Q),
   split_at(P1, Outer0, Left, Right0),
   (  Q1 >= P1
   -> QQ is Q1 - P1+1,
      split_at(QQ, Right0, _, Right)
   ;  Right = Right0
   ),
   append(Left, [V|Right], Outer),
   % modify graph
   add_vertices(G0, [V], G1),
   findall(E, (between(P, Q, K), nth0(K, Outer0, X), (E=V-X; E=X-V)), NewEdges),
   add_edges(G1, NewEdges, G).

add_point_rec(O, O, K, K, G, G, Outer, Outer) :- !.
add_point_rec(O0, O, K0, K, G0, G, Outer0, Outer) :-
   succ(K0, K1),
   add_point(O0, O1, K1, G0, G1, Outer0, Outer1), 
   add_point_rec(O1, O, K1, K, G1, G, Outer1, Outer). 

:- meta_predicate random_triangulated_planar_graph(:, -, +, -, -).

%% random_triangulated_planar_graph(+Opts0, -Opts, +K, -Graph, -Outer)
%
% It is det/nondet depending on Opts0
random_triangulated_planar_graph(O, O, 3, G, Outer) :- !,
   Outer=[1,3,2],
   vertices_edges_to_ugraph(Outer, [1-2, 2-1, 2-3, 3-2, 3-1, 1-3], G).
random_triangulated_planar_graph(O0, O, K, G, Outer) :-
   K >= 3,   
   random_triangulated_planar_graph(O0, O1, 3, G0, Outer0),
   add_point_rec(O1, O, 3, K, G0, G1, Outer0, Outer),
   add_outer_triangles(G1, G).

add_outer_triangles(G, G). % FIXME