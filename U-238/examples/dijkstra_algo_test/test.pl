:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(u(gt/gt_graphs)).
:- use_module(u(gt/gt_numbers)).

all_branches(N_Nodes, Branches) :-
   findall(A-B,
           ( between(1, N_Nodes, A),
             succ(A, A1),
             between(A1, N_Nodes, B) ),
           Branches).

random_options(I,
               [generator(randgen:pcg32_1),
                rand_state(pcg32_init(I, 1))
               ]).

%Usage:
% N = 12, random_options(2, O0), new(@p, dialog(graph)), send(@p, open), send(@p, append(button(cancel, message(@p, return)))), show_min_rand_path(@p, [nondet|O0], N), get(@p, confirm, _), free(@p).

show_min_rand_path(Ref, O0, N) :-
   random_triangulated_planar_graph(O0, O1, N, G, _, DE, Emb),
   [Start, Stop] ins 1..N,
   Stop #> Start+3,
   random_number(O1, O2, Start),
   random_number(O2, O3, Stop),
   aggregate_all(min(D, RandPath),
                 ( random_path(O3, _, G, Start, Stop, RandPath),
                   path_to_edges(RandPath, [], Es),
                   embedded_distance(Es, Emb, D)),
                 min(_,MinRandPath)),
   writeln(MinRandPath),
   draw_graph(Ref, G, DE, Emb),
   draw_path(Ref, MinRandPath, Emb).

