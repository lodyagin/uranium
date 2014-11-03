% -*- fill-column: 58; -*-
%
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
           class_diagram/1,  % +Options
           class_diagram/2,  % +Stream, +Options
           class_fields/0,
           class_graph/3     % @Only_Class, ?Is_Primary, -Class_Graph
          ]).

:- use_module(u(v)).
:- use_module(u(ur_option)).
:- use_module(u(internal/objects_i)).
:- use_module(library(ugraphs)).

:- dynamic fields/3. %Name, Class, Prefix_Class

class_diagram :-
   class_diagram([]).

class_diagram(Options) :-
   current_output(Out),
   class_diagram(Out, Options).

% class_diagram(+Stream, +Options) is det.
%
% Prints the class inheritance diagram to Stream.
% Options:
% is_primary(true|false|_) - false for rebased
% classes.
% only_class(_|ClassName) - if ClassName nonvar show only
% inheritance where ClassName is either a parent or a child.
class_diagram(Stream, Options) :-
   options_object(class_diagram, Options, Opt),
   Opt / [is_primary, only_class] ^= [is_primary(Is_Primary), only_class(Only_Class)], !,
   (  class_graph(Only_Class, Is_Primary, Graph),
      format(Stream, 'is_primary(~p):\n', [Is_Primary]),
      top_sort(Graph, Order),
      (  member(Parent_Id, Order),
         class_id(Parent_Id, Parent),
         format(Stream, '~a[~d] ->\t', [Parent, Parent_Id]),
         (  memberchk(Parent_Id - Children_Ids, Graph),
            member(Class_Id, Children_Ids),
            class_id(Class_Id, Class),
            class_new_fields(Class_Id, New_Fields),
            format(Stream, '~a[~d] (+~w)\t', [Class, Class_Id, New_Fields]),
            fail ; true ),
         nl,
         fail ; true
      ),
      fail ; true
   ).

% class_graph(@Only_Class, ?Is_Primary, -Class_Graph) is
% undet.
%
% Returns the class diagram as ugraph.
% 
% @param Is_Primary is {true, false} --> it is det.  In other
% case return graphs for different values of Is_Primary.
% @param Only_Class if bound that limits graph to this
% class inheritance only.
class_graph(Only_Class, Is_Primary, Class_Graph) :-
   var(Only_Class), !,
   bagof(Parent_Id - Class_Id,
         Class^(objects:parent_(Class_Id, Parent_Id),
          nonvar(Parent_Id),
          objects:class_id(Class_Id, Is_Primary, Class)
         ), Edges),
   vertices_edges_to_ugraph([], Edges, Class_Graph).
class_graph(Class, Is_Primary, Class_Graph) :-
   bagof(Parent_Id - Class_Id,
         (  objects:class_id(Id, Is_Primary, Class),
            ( Id = Parent_Id ; Id = Class_Id ),
            objects:parent_(Class_Id, Parent_Id)
         ), Edges),
   vertices_edges_to_ugraph([], Edges, Class_Graph).

class_fields :-
   class_graph(_, true, Graph),
   top_sort(Graph, Order),
   (  member(Class_Id, Order),
      class_fields(Class_Id),
      fail ; true ).

class_fields(Class_Id) :-
   class_id(Class_Id, Class),
   format('~a[~d]:\t', [Class, Class_Id]),
   class_fields2(Class_Id),
   nl, nl.

class_fields2(0) :- !.

class_fields2(Class_Id) :-
   objects:parent_(Class_Id, Parent_Id),
   class_fields2(Parent_Id),
   class_new_fields(Class_Id, Fields),
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

   class_graph(_, true, Graph),
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
