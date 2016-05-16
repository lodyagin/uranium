% Operating on graphs.

:- module(ur_graphs,
          [is_complete_graph/1,
           is_undirected_connected_graph/1,
           is_undirected_graph/1,
           is_undirected_path_graph/1,
           degree/3, % +Vertex, +Graph, -Degree
           shortest_path/5 % :GetDistance, +G, +P, +Q, -Path
          ]).

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ugraphs)).
:- use_module(u(massoc)).

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

:- meta_predicate shortest_path(:, +, +, +, -).

% shortest_path(:GetDistance, +G, +P, +Q, -Path) is det.
shortest_path(GetDistance, G, P, Q, [P|Path1]) :-
   must_be(callable, GetDistance),
   must_be(nonvar, G),
   must_be(nonvar, P),
   must_be(nonvar, Q),
   empty_edges(E2_0),
   shortest_path_int(GetDistance, G,
                     [P], _, % set A
                     [], _,  % set B
                     [], E1, % set E1
                     E2_0, _,    % set E2
                     Q, P, 0.0),
   debug(shortest_path, "E1 is ~p", [E1]),
   reconstruct_path(E1, Q, [], Path1).

shortest_path_int(_, _, A, A, B, B, E1, E1, E2, E2, Q, Q, _) :-
   !. % repeat the process until node Q is transfered to set A.
shortest_path_int(GetDistance, G, A0, A, B0, B, E1_0, E1, E2_0, E2,
                  Q, Last0, PLast0D) :-
   debug(shortest_path, "the last point is ~p", [Last0]),
   neighbours(Last0, G, Rs),   
   shortest_path_step1(GetDistance, Rs, A0, B0, B1, E2_0, E2_1,
                       Last0, PLast0D),
   shortest_path_step2(A0, A1, B1, B2, E1_0, E1_1, E2_1, E2_2,
                       Last, PLastD),
   shortest_path_int(GetDistance, G, A1, A, B2, B, E1_1, E1, E2_2, E2,
                     Q, Last, PLastD).
   
% Step1. Consider all branches Last-R connecting the node
% just transferred to set A (Last) with nodes R in sets B or C.
shortest_path_step1(_, [], A, B, B, E2, E2, _, _) :-
   !,
   debug(shortest_path, "step 1 done, A is ~p, B is ~p", [A, B]).
shortest_path_step1(GetDistance, [R|TR], A, B0, B, E2_0, E2,
                    Last, PLastD) :-
   debug(shortest_path, "analyze the branch ~p-~p", [Last, R]),  
   (   ord_memberchk(R, B0)
   ->  % If node R belongs to set B, we investigate whether the use
       % of branch Last-R gives rise to a shorter path from P to R
       % than the known path that uses the corresponding branch
       % in set E2. 
       debug(shortest_path, "~p is in B set", [R]),
       call(GetDistance, Last, R, LastRD),
       PRDNew is PLastD + LastRD,
       get_edge(R, E2_0, OldA, PRDOld),
       debug(shortest_path, "get a new distance ~f vs old ~f",
             [PRDNew, PRDOld]),
       (  PRDNew < PRDOld
       -> % branch Last-R replaces the corresponding branch
          % in set E2 and the latter is rejected.
          replace_edge(OldA, Last, R, E2_0, E2_1, PRDNew),
          debug(shortest_path,
                "~p-~p (~f) is replaced with ~p-~p (~f) in E2",
                [OldA, R, PRDOld, Last, R, PRDNew])
       ;  % If this is not so, branch Last-R is rejected
          debug(shortest_path, "keep ~p-~p in E2", [OldA, R]),
          E2_1 = E2_0
       ),
       B1 = B0
   ;   \+ ord_memberchk(R, A)
   ->
       % If the node R belongs to set C, it is added to set B
       % and branch Last-R is added to set E2.
       debug(shortest_path, "~p is in C set", [R]),
       call(GetDistance, Last, R, LastRD),
       ord_add_element(B0, R, B1),
       PRD is PLastD + LastRD,
       put_edge(Last, R, E2_0, PRD, E2_1),
       debug(shortest_path, "~p-~p is added to E2", [Last, R])
   ;
       B1 = B0, E2_1 = E2_0
   ),
   shortest_path_step1(GetDistance, TR, A, B1, B, E2_1, E2,
                       Last, PLastD).

shortest_path_step2(A0, A, B0, B, E1, [V1-V2|E1], E2_0, E2,
                    V2, PV2Dist) :-
   % The node with minimum distance from starting vertex is
   % transfered from set B to set A and the corresp. branch from
   % E2 to E1.
   get_min_dist_edge(E2_0, V1, V2, PV2Dist),
   ord_del_element(B0, V2, B), ord_add_element(A0, V2, A),
   del_edge(V1, V2, E2_0, E2),
   debug(shortest_path,
         "~p is transfered to A, ~p is transfered to E1",
         [V2, V1-V2]).

reconstruct_path([], _, Path, Path) :- !.
reconstruct_path([A-B|T], B, P0, P) :-
   !, reconstruct_path(T, A, [B|P0], P).
reconstruct_path([_|T], B, P0, P) :-
   reconstruct_path(T, B, P0, P).

empty_edges(edges(Es, Ds)) :-
   empty_assoc(Es),
   empty_massoc(Ds).

get_min_dist_edge(edges(Edges, DistOrder), V1, V2, MinDist) :-
   min_massoc(DistOrder, MinDist, V2), !,
   get_assoc(V2, Edges, p(V1, MinDist)).

% OldA-B -> NewA-B
replace_edge(OldA, NewA, B,
           edges(Edges0, DistOrder0),
           edges(Edges, DistOrder),
           Distance) :-
   get_assoc(B, Edges0, p(OldA, OldDist), Edges, p(NewA, Distance)),
   once(del_massoc(OldDist, DistOrder0, B, DistOrder1)),
   put_massoc(Distance, DistOrder1, B, DistOrder).

get_edge(B, edges(Edges, _), A, Dist) :-
   get_assoc(B, Edges, p(A, Dist)).

put_edge(A, B,
   edges(Edges0, DistOrder0), Dist, edges(Edges, DistOrder)) :-
   put_assoc(B, Edges0, p(A, Dist), Edges),
   put_massoc(Dist, DistOrder0, B, DistOrder).

del_edge(A, B, edges(Edges0, DistOrder0), edges(Edges, DistOrder)) :-
   del_assoc(B, Edges0, p(A, OldDist), Edges),
   once(del_massoc(OldDist, DistOrder0, B, DistOrder)).
