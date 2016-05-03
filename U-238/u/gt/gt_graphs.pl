:- module(gt_graphs,
          [random_triangulated_planar_graph/6 % +Opts0, -Opts, +K,
                                              % -Graph, -Outer, -Emb)
           ]).

:- use_module(library(dialect/hprolog)).
:- use_module(library(ugraphs)).
:- use_module(u(gt/gt_numbers)).
:- use_module(u(v)).

%
% Adds a new vertex in canonical order
% to a triangulated planar graph
add_point(O0, O, V, G0, G, Outer0, Outer, Emb0, Emb) :-
   succ(N, V),
   M in 2..N, % the number of covered points
   random_number(O0, O1, M),
   PMax is N - M + 1,
   P in 1..PMax,
   random_number(O1, O, P),
   Q is P + M - 1, succ(P, P1), succ(Q1, Q),
   succ(P10, P1),
   split_at(P10, Outer0, Left, Right0),
   (  Q1 >= P1
   -> QQ is Q1 - P1,
      split_at(QQ, Right0, _, Right)
   ;  Right = Right0
   ),
   append(Left, [V|Right], Outer),
   % modify graph
   add_vertices(G0, [V], G1),
   findall(E, (between(P, Q, K), nth1(K, Outer0, X), (E=V-X; E=X-V)), NewEdges),
   add_edges(G1, NewEdges, G),
   % modify embedding
   shift(P, Q, Emb0, Emb).

add_point_rec(O, O, K, K, G, G, Outer, Outer, Emb, Emb) :- !.
add_point_rec(O0, O, K0, K, G0, G, Outer0, Outer, Emb0, Emb) :-
   succ(K0, K1),
   add_point(O0, O1, K1, G0, G1, Outer0, Outer1, Emb0, Emb1), 
   add_point_rec(O1, O, K1, K, G1, G, Outer1, Outer, Emb1, Emb). 

:- meta_predicate random_triangulated_planar_graph(:, -, +, -, -, -).

%% random_triangulated_planar_graph(+Opts0, -Opts, +K, -Graph,
%%                                  -Outer, -Emb)
%
% It is det/nondet depending on Opts0
random_triangulated_planar_graph(O0, O, K, G, Outer, Emb) :-
   obj_construct(grid_embedding_v, [], [], Emb0),
   random_triangulated_planar_graph_int(O0, O, K, G, Outer, Emb0, Emb).

random_triangulated_planar_graph_int(O, O, 3, G, Outer, Emb, Emb) :- !,
   Outer=[1, 3, 2],
   vertices_edges_to_ugraph(Outer,
      [1-2, 2-1, 2-3, 3-2, 3-1, 1-3], G),
   obj_field(Emb, coords, [p(0, 0), p(2, 0), p(1, 1)]).
random_triangulated_planar_graph_int(O0, O, K, G, Outer, Emb0, Emb) :-
   K >= 3,   
   random_triangulated_planar_graph_int(O0, O1, 3, G0, Outer0, Emb0,
                                        Emb1),
   add_point_rec(O1, O, 3, K, G0, G1, Outer0, Outer, Emb1, Emb),
   add_outer_triangles(G1, G).

add_outer_triangles(G, G). % FIXME

% This is Fraysseix's shift algorithm implementation
shift(P, Q, Emb0, Emb) :-
   P >= 1, Q > P,
   obj_rewrite(Emb0, [coords], [Coords0], [Coords], Emb),
   shift_left(P, Coords0, Left1, [WP|Right1]),
   QQ is Q - P - 1,
   split_at(QQ, Right1, Middle, [WQ2|Right2]),
   shift_right([WQ2|Right2], [WQ|Right3]),
   append([Left1, [WP], Middle, [WQ|Right3], [Vk]], Coords),
   mu(WP, WQ, Vk).

%% shift_left(+P, +Coords, -Shifted, -Rest) is det.
%
% shifts P first coordinates one point left. 
shift_left(1, [p(X, Y)|T], [], [p(XS, Y)|T]) :- !,
   XS is X - 1.
shift_left(P, [p(X, Y)|Coords], [p(XS, Y)|LeftShifted], Right) :-
   XS is  X - 1,
   succ(P1, P),
   shift_left(P1, Coords, LeftShifted, Right).

%% shift_right(+Coords, -Shifted) is det.
%
% shifts Coords one point right
shift_right([], []) :- !.
shift_right([p(X, Y)|Coords], [p(XS, Y)|Shifted]) :-
   succ(X, XS),
   shift_right(Coords, Shifted).

mu(p(X1, Y1), p(X2, Y2), p(X, Y)) :-
   X is (X1 - Y1 + X2 + Y2) / 2,
   Y is (-X1 + Y1 + X2 + Y2) / 2.

draw_graph(P, G, Emb) :-
   obj_field(Emb, norm_coords, Coords),
   draw_vertices(P, Coords),
   edges(G, Edges),
   draw_edges(P, Edges, Coords).

draw_vertices(_, []) :- !.
draw_vertices(Ref, [P|T]) :-
   calc_display_coord(P, disp(X, Y)),
   send(Ref, display, new(V, circle(12)), point(X, Y)),
   send(V, fill_pattern, colour(red)),
   draw_vertices(Ref, T).

draw_edges(_, [], _) :- !.
draw_edges(P, [E|T], Coords) :-
   draw_edge(P, E, Coords),
   draw_edges(P, T, Coords).

draw_edge(Ref, A-B, Coords) :-
   nth1(A, Coords, PA),
   nth1(B, Coords, PB),
   calc_display_coord(PA, disp(XA, YA)),
   calc_display_coord(PB, disp(XB, YB)),
   send(Ref, display, new(_, line(XA, YA, XB, YB, second))).

calc_display_coord(p(X0, Y0), disp(X, Y)) :-
   X is X0 * 50 + 10 + 150,
   Y is Y0 * 50 + 10 + 150.
      
   