:- module(class_diagram, [class_diagram/0]).

:- use_module(library(ur_objects)).
:- use_module(library(internal/objects_i)).
:- use_module(library(ugraphs)).

class_diagram :-

   class_graph(Graph),
   top_sort(Graph, Order),
   (  member(Parent, Order),
      format('~a ->\t', Parent),
      class_primary_id(Parent, Parent_Id),
      (  objects:parent(Class_Id, Parent_Id2),
         nonvar(Parent_Id2), Parent_Id2 = Parent_Id,
         class_primary_id(Class, Class_Id),
         class_new_fields(Class_Id, New_Fields),
         format('~a (+~w)\t', [Class, New_Fields]),
         fail ; true ),
      nl,
      fail ; true
   ).
   
class_graph(Class_Graph) :-

   findall(Parent - Class,

           (objects:parent(Class_Id, Parent_Id),
            nonvar(Parent_Id),
            class_primary_id(Class, Class_Id),
            class_primary_id(Parent, Parent_Id)),

           Edges
          ),
   vertices_edges_to_ugraph([], Edges, Class_Graph).
   