:- begin_tests(ur_graphs).

:- use_module(library(clpfd)).
:- use_module(library(ugraphs),
              [vertices_edges_to_ugraph/3]).
:- use_module(u(ur_graphs)).
:- use_module(u(gt/gt_graphs)).
:- use_module(u(gt/gt_lists)).
:- use_module(u(gt/gt_numbers)).

test(is_undirected_graph_positive) :-
   vertices_edges_to_ugraph([1,2,3],[1-2, 2-3, 1-3, 3-1, 2-1, 3-2], G),
   is_undirected_graph(G).

test(is_undirected_graph_negative, fail) :-
   vertices_edges_to_ugraph([1,2,3],[1-2, 2-3, 1-3, 3-1], G),
   is_undirected_graph(G).

test(is_complete_graph, N == 1) :-
   P = 5,
   Q is P * (P-1) / 2,
   % generate all branches
   findall(A-B,
           ( between(1, P, A),
             succ(A, A1),
             between(A1, P, B) ),
           Bs),
   length(Bs, Q),
   aggregate_all(count,
                 (random_sublist(Bs, [nondet, length(1..Q)], _, S),
                  findall(X, between(1, P, X), V),
                  findall(X, (member(A-B, S), (X=A-B; X=B-A)), E),
                  vertices_edges_to_ugraph(V, E, G),
                  is_complete_graph(G)
                 ),
                 N).

test(shortest_path, Path == MinRandPath) :-
   N=15, DE = [],
   % Generate the (random) graph and the answer
   O0 = [nondet,
         generator(randgen:pcg32_1),
         rand_state(pcg32_init(1, 1))],
   random_triangulated_planar_graph(O0, O1, N, G, DE, _, Emb),
   [Start, Stop] ins 1..N,
   Stop #> Start+3,
   random_number(O1, O2, Start), random_number(O2, O3, Stop),
   aggregate_all(min(D, RandPath),
                 ( random_path(O3, _, G, Start, Stop, RandPath),
                   path_to_edges(RandPath, [], Es),
                   embedded_distance(Es, Emb, D)),
                 min(_,MinRandPath)), !,
   % Visualization (debug)
%   new(Dlg, dialog(graph)),
%   send(Dlg, open),
%   send(Dlg, append(button(cancel, message(Dlg, return)))),
%   draw_graph(Dlg, G, DE, Emb),
%   draw_path(Dlg, MinRandPath, Emb),
%   get(Dlg, confirm, _),
   % Test
   shortest_path(get_distance(Emb), G, Start, Stop, Path).

get_distance(Emb, A, B, D) :-
   embedded_distance([A-B], Emb, D).

:- end_tests(ur_graphs).