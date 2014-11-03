%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(class_diagram,
          [all_fields/0,
           class_diagram/0,
           class_fields/0
          ]).

:- use_module(u(v)).
:- use_module(u(internal/objects_i)).
:- use_module(library(ugraphs)).

:- dynamic fields/3. %Name, Class, Prefix_Class

class_diagram :-

   class_graph(Graph),
   top_sort(Graph, Order),
   (  member(Parent_Id, Order),
      class_id(Parent_Id, Parent),
      format('~a[~d] ->\t', [Parent, Parent_Id]),
      (  objects:parent_(Class_Id, Parent_Id),
         class_id(Class_Id, Class),
         class_new_fields(Class_Id, New_Fields),
         format('~a[~d] (+~w)\t', [Class, Class_Id, New_Fields]),
         fail ; true ),
      nl,
      fail ; true
   ).

class_graph(Class_Graph) :-
   findall(Parent_Id - Class_Id,
           (objects:parent_(Class_Id, Parent_Id),
            nonvar(Parent_Id)),
           Edges
          ),
   vertices_edges_to_ugraph([], Edges, Class_Graph).


class_fields :-

   class_graph(Graph),
   top_sort(Graph, Order),
   (  member(Class, Order),
      class_fields(Class),
      fail ; true ).

class_fields(Class) :-

   format('~a:\t', Class),
   class_fields2(Class),
   nl, nl.

class_fields2(object_base_v) :- !.

class_fields2(Class) :-

   class_parent(Class, Parent),
   class_fields2(Parent),
   class_fields_new(Class, Fields),
   maplist(format('~a|'), Fields),
   write('|').


all_fields :-

   build_fields_db,
   setof(A/C, B^fields(A, B, C), List),
   print_fields(List).

print_fields(['#'/_|T]) :- !,
   print_fields(T).

print_fields([Name/Prefix|T]) :-
   var(Prefix), !,
   writeln(Name),
   print_fields(T).

print_fields([Name/Prefix|T]) :-
   format('  ~a:~a\n', [Prefix, Name]),
   print_fields(T).

build_fields_db :-

   retractall(fields(_, _, _)),
   assert(fields('#', '#', '#')),

   class_graph(Graph),
   top_sort(Graph, Order),
   (  member(Class, Order),
      add_fields_to_db(Class),
      fail ; true ).

add_fields_to_db(Class) :-

   class_fields_new(Class, Fields),
   add_fields_to_db(Class, Fields).

add_fields_to_db(_, []) :- !.

add_fields_to_db(Class, [Field|T]) :-

   (  fields(Field, _, _)
   -> (  retract(fields(Field, Orig_Class, _)),
         assert(fields(Field, Orig_Class, Orig_Class)),
         fail ; true ),
      assert(fields(Field, Class, Class))
   ;  assert(fields(Field, Class, _))
   ),
   add_fields_to_db(Class, T).
