:- begin_tests(tarjan).
:- use_module(library(ugraphs)).
:- use_module(u(algorithm/tarjan)).
:- use_module(u(v)).
:- use_module(u(vd)).

test(tarjan1,
     [SCC == [[1,2,3,10,11,12,13],
              [4],
              [5],
              [6],
              [7],
              [8,9],
              [14],
              [15],
              [16]]
      ]) :-

   obj_construct(tarjan_test_vertex_v,
                 [vertex_id], [1], Start0),
   obj_rebase((object_v -> tarjan_vertex_v), Start0, Start),
   
   db_clear(tarjan_test),
   tarjan(tarjan_test,
          tarjan_test_vertex_v, vertex_id,
          load_vertex, resolve_destinations(test_graph1),
          Start, SCC0),
   sort(SCC0, SCC).

test_graph1(Graph) :-

   Graph = [1  - [2, 4, 10],
            2  - [3],
            3  - [1],
            4  - [4, 5],
            5  - [6, 8],
            6  - [7],
            7  - [],
            8  - [7, 9],
            9  - [8],
            10 - [11, 13],
            11 - [12, 14],
            12 - [10, 16],
            13 - [3],
            14 - [14, 15],
            15 - [],
            16 - []
           ].

load_vertex(Id, Obj) :-

   obj_construct(tarjan_test_vertex_v,
                 [vertex_id], [Id], Obj0),
   obj_rebase((object_v -> tarjan_vertex_v), Obj0, Obj).

resolve_destinations(Pred, V, W_Ids) :-

   call(Pred, Graph),
   obj_field(V, vertex_id, V_Id),
   neighbours(V_Id, Graph, W_Ids).

:- end_tests(tarjan).
