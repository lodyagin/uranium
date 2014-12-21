% -*- fill-column: 58; -*-
% _________________________________________________________
%
% This file is a part of Uranium, a general-purpose prolog
% library.
%
% This library is free software; you can redistribute it
% and/or modify it under the terms of the GNU Lesser
% General Public License as published by the Free Software
% Foundation; either version 2.1 of the License, or (at
% your option) any later version.
%
% This library is distributed in the hope that it will be
% useful, but WITHOUT ANY WARRANTY; without even the
% implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser
% General Public License along with this library; if not,
% write to the Free Software Foundation, Inc., 51 Franklin
% Street, Fifth Floor, Boston, MA 02110-1301 USA
% _________________________________________________________
%
% The original implementation of breath-first-search taken
% from the book "The Craft of Prolog" by Richard O'Keefe.
% Modified by Sergei Lodyagin
% _________________________________________________________

:- module(bfs,
          [bfs_graph/3
          ]).

:- use_module(library(ordsets)).
:- use_module(library(error)).
%:- use_module(u(internal/check_arg)).
:- use_module(u(queue)).


:- meta_predicate bfs_graph(2, +, -).

% bfs_graph(:Children, +Start, -Answer) is nondet.
%
% Performs breath-first-search in a graph. Use
% Children(Node, ChildList) and Start as the graph
% definition.

bfs_graph(Children, Start, Answer) :-
%  Ctx = context(bfs/?, _),
%  check_inst(Start, Ctx),
  must_be(callable, Children),

  queue(Start, Open),
  bfs_graph_star(Children, Open, [Start], Answer).

bfs_graph_star(Children, Open, Closed, Y) :-
  queue_head(X, Open1, Open),
  (  Y = X
  ;  call(Children, X, ChildList)
     ord_union(Closed, ChildList, Closed1, New),
     queue_last_list(New, Open1, Open2),
     bfs_graph_star(Open2, Closed1, Y)
  ).
   