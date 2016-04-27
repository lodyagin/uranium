:- begin_tests(ur_graphs).

:- use_module(library(ugraphs)).
:- use_module(u(ur_graphs)).
:- use_module(u(gt/gt_lists)).

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


:- end_tests(ur_graphs).