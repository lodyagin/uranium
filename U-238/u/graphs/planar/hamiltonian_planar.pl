:- module(hamiltonian_planar,
          []).

%:- use_module(u(ur_subarrays)).
:- use_module(u(queue)).

connect(V, V, L, E, E) -->
   { L =< 3 }, !, [].
connect(V0, V, L, E0, E) -->
   "A", !,
   { append([A, _, B], V1, V0),
     append([A, B], V1, V2),
     succ(L1, L),
     E1 = [A-B|E0]
   },
   connect(V2, V, L1, E1, E).
connect(V0, V, L, E0, E) -->
   "a", !,
   { rotate1(V0, V1),
     append([A, _, B], V2, V1),
     append([A, B], V2, V3),
     succ(L1, L),
     E1 = [A-B|E0]
   },
   connect(V3, V, L1, E1, E).
connect(V0, V, L, E0, E) -->
   "B", !,
   { V0 = [A|V1],
     reverse(V1, V2),
     V3 = [A|V2],
     append([A, _, B], V4, V3),
     append([A, B], V4, V5),
     succ(L1, L),
     E1 = [A-B|E0]
   },
   connect(V5, V, L1, E1, E). 
connect(V0, V, L, E0, E) -->
   "b", !,
   { append([A, B, _, C], V1, V0),
     reverse(V1, V2),
     append([C, B, A], V2, V3),
     succ(L1, L),
     E1 = [C-B|E0]
   },
   connect(V3, V, L1, E1, E).
