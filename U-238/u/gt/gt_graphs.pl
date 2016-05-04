:- module(gt_graphs,
          [random_triangulated_planar_graph/7, % +Opts0, -Opts, +K,
                                    % -Graph, -DoubleEdges, -Outer, -Emb)
           draw_graph/4
           ]).

:- use_module(library(dialect/hprolog)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- use_module(u(gt/gt_numbers)).
:- use_module(u(v)).

%
% Adds a new vertex in canonical order
% to a triangulated planar graph
add_point(O0, O, V, G0, G, Outer0, Outer, Emb0, Emb) :-
   length(Outer0, N),
   M in 2..N, % the number of covered points
   random_number(O0, O1, M),
   PMax is N - M + 1,
   P in 1..PMax,
   random_number(O1, O, P),
   Q is P + M - 1, 
   succ(P0, P),
   split_at(P0, Outer0, Left, [VP|Right0]),
   (  Q > P
   -> QQ is Q - P - 1,
      split_at(QQ, Right0, _Middle, [VQ|Right])
   ;  Right = Right0
   ),
   append(Left, [VP, V, VQ|Right], Outer),
   % modify graph
   add_vertices(G0, [V], G1),
   findall(E, (between(P, Q, K), nth1(K, Outer0, X), (E=V-X; E=X-V)), NewEdges),
   add_edges(G1, NewEdges, G),
   % modify embedding
   shift(VP, VQ, Emb0, Emb).

add_point_rec(O, O, K, K, G, G, Outer, Outer, Emb, Emb) :- !.
add_point_rec(O0, O, K0, K, G0, G, Outer0, Outer, Emb0, Emb) :-
   succ(K0, K1),
   add_point(O0, O1, K1, G0, G1, Outer0, Outer1, Emb0, Emb1), 
   add_point_rec(O1, O, K1, K, G1, G, Outer1, Outer, Emb1, Emb). 

:- meta_predicate random_triangulated_planar_graph(:, -, +, -, -, -).

%% random_triangulated_planar_graph(+Opts0, -Opts, +K, -Graph,
%%                                  -DoubleEdges, -Outer, -Emb)
%
% It is det/nondet depending on Opts0
random_triangulated_planar_graph(O0, O, K, G, DoubleEdges, Outer, Emb) :-
   obj_construct(grid_embedding_v, [curved_edges], [CurvedEdges], Emb0),
   random_triangulated_planar_graph_int(O0,O1, K, G1, Outer1, Emb0, Emb),
   length(Outer1, NO),
   add_outer_triangles(O1, O, NO, Outer1, Outer, G1, G,
                       [], DoubleEdges, [], CurvedEdges).
   %O1 = O, Outer1 = Outer, G1 = G, [] = DoubleEdges, [] = CurvedEdges.

random_triangulated_planar_graph_int(O, O, 3, G, Outer, Emb, Emb) :- !,
   Outer=[1, 3, 2],
   vertices_edges_to_ugraph(Outer,
      [1-2, 2-1, 2-3, 3-2, 3-1, 1-3], G),
   obj_field(Emb, coords, [p(0, 0), p(2, 0), p(1, 1)]).
random_triangulated_planar_graph_int(O0, O, K, G, Outer, Emb0, Emb) :-
   K >= 3,   
   random_triangulated_planar_graph_int(O0, O1, 3, G0, Outer0, Emb0,
                                        Emb1),
   add_point_rec(O1, O, 3, K, G0, G, Outer0, Outer, Emb1, Emb).

add_outer_triangles(O, O, 3, Outer, Outer, G, G, DE, DE, CE, CE) :- !.
add_outer_triangles(O0, O, NO0, Outer0, Outer, G0, G, DE0, DE, CE0, CE):-
   NO0 > 3,
   succ(NO1, NO0),
   P in 0..NO1,
   (  NO0 =:= 4
   -> once(random_number(O0, O1, P))
   ;  random_number(O0, O1, P)
   ),
   cover_outer_point(P, NO1, Outer0, Outer1, G0, G1, DE0, DE1, CE0, CE1),
   add_outer_triangles(O1, O, NO1, Outer1, Outer, G1, G, DE1, DE,CE1,CE).

cover_outer_point(P, NewN, Outer0, Outer, G0, G, DE0, DE, CE0, CE) :-
   append(Outer0, Cycle, Cycle),
   P1 is P + NewN,
   succ(NewN, OldN),
   split_at(P1, Cycle, L, [VP, _, VQ|R]),
   append(L, [VP], L1),
   append(L1, [VQ|R], Cycle1),
   drop(OldN, Cycle1, Cycle2),
   take(NewN, Cycle2, Outer),
   add_edges(G0, [VP-VQ, VQ-VP], G),
   (  G == G0
   -> % double edges
      ord_add_element(DE0, VP-VQ, DE1),
      ord_add_element(DE1, VQ-VP, DE)
   ;  DE=DE0
   ),
   ord_add_element(CE0, VP-VQ, CE1),
   ord_add_element(CE1, VQ-VP, CE).
   
% This is Fraysseix's shift algorithm implementation
shift(WP, WQ, Emb0, Emb) :-
   obj_rewrite(Emb0, [coords], [Coords0], [Coords], Emb),
   nth1(WQ, Coords0, p(WQX0, WQY)),
   nth1(WP, Coords0, p(WPX, WPY)),
   shift_right2(WQX0, Coords0, Coords1),
   WQX is WQX0 + 2,
   shift_right1(WPX, WQX, Coords1, Coords2),
   mu(p(WPX, WPY), p(WQX, WQY), VC),
   append(Coords2, [VC], Coords).

%% shift_right2(+StartX, +Coords, -Shifted) is det.
%
% shifts Coords two points right
shift_right2(_, [], []) :- !.
shift_right2(WQX, [p(X0, Y)|T0], [p(X, Y)|T]) :-
   (  X0 >= WQX
   -> X is X0 + 2
   ;  X = X0
   ),
   shift_right2(WQX, T0, T).

%% shift_right1(+StartX, +StopX, +Coords, -Shifted) is det.
%
% shifts Coords one point right
shift_right1(_, _, [], []) :- !.
shift_right1(WPX, WQX, [p(X0, Y)|T0], [p(X, Y)|T]) :-
   succ(WPX, WPX1),
   succ(WQX1, WQX),
   (  between(WPX1, WQX1, X0)
   -> succ(X0, X)
   ;  X = X0
   ),
   shift_right1(WPX, WQX, T0, T).

mu(p(X1, Y1), p(X2, Y2), p(X, Y)) :-
   X is (X1 - Y1 + X2 + Y2) / 2,
   Y is (-X1 + Y1 + X2 + Y2) / 2.

draw_graph(P, G, DE, Emb) :-
   obj_unify(Emb, [coords, curved_edges], [Coords, CurvedEdges]),
   draw_vertices(P, Coords),
   edges(G, Edges),
   draw_edges(P, Edges, DE, Coords, CurvedEdges).

draw_vertices(_, []) :- !.
draw_vertices(Ref, [P|T]) :-
   calc_display_coord(P, disp(X, Y)),
   send(Ref, display, new(V, circle(12)), point(X, Y)),
   send(V, fill_pattern, colour(red)),
   draw_vertices(Ref, T).

draw_edges(_, [], _, _, _) :- !.
draw_edges(P, [E|T], DoubleEdges, Coords, CurvedEdges0) :-
   ( ord_memberchk(E, CurvedEdges0) -> DrawCurved = t ; DrawCurved = f ),
   ( ord_memberchk(E, DoubleEdges) -> DrawStraight = t; true ),
   ( DrawCurved == t -> ignore(DrawStraight = f); DrawStraight = t),

   (  DrawCurved == t
   -> ord_del_element(CurvedEdges0, E, CurvedEdges1),
      E=EP-EQ,
      (  EP < EQ % draw once (undirected)
      -> draw_edge(curved, P, E, Coords)
      ; true )
   ;
      CurvedEdges1 = CurvedEdges0
   ),

   (  DrawStraight == t
   -> draw_edge(straight, P, E, Coords)
   ;  true 
   ),
   draw_edges(P, T, DoubleEdges, Coords, CurvedEdges1).

draw_edge(Type, Ref, A-B, Coords) :-
   nth1(A, Coords, PA),
   nth1(B, Coords, PB),
   calc_display_coord(PA, disp(XA, YA)),
   calc_display_coord(PB, disp(XB, YB)),
   (  Type == straight
   -> new(Arc, line(XA, YA, XB, YB, second))
   ;  new(Arc, arc),
      send(Arc, points(XA, YA, XB, YB, 25)),
      send(Arc, colour(blue))
   ),
   send(Ref, display, Arc).

calc_display_coord(p(X0, Y0), disp(X, Y)) :-
   X is X0 * 50 + 60,
   Y is Y0 * 50 + 60.
      
   