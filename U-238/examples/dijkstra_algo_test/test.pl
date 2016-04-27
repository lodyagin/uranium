:- use_module(library(error)).

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
   