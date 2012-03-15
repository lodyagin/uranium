% -*- fill-column: 65; -*-
% _____________________________________________________________
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% _____________________________________________________________

:- module(tarjan, [tarjan/7]).

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(v)).
:- use_module(u(vd)).

:- meta_predicate tarjan(+, +, +, 2, 2, +, -).


tarjan(DB,
       Vertex_Functor, Vertex_Id_Fld,
       Load_Vertex, Resolve_Destinations,
       Start_Vertex,
       Scc) :-

   Ctx = context(tarjan/7, _),
   check_inst(Vertex_Functor, Ctx),
   check_inst(Start_Vertex, Ctx),
   check_db_key(DB, Ctx),
   check_field_name(Vertex_Id_Fld, Ctx),
   check_existing_class_arg(Vertex_Functor, Ctx),

   must_be(callable, Load_Vertex),
   must_be(callable, Resolve_Destinations),

   check_object_arg(Start_Vertex, Ctx, _),
   
   scc(DB,
       Vertex_Functor, Vertex_Id_Fld,
       Load_Vertex, Resolve_Destinations,
       Start_Vertex, _,
       [], _, 0, _, [], Scc).

scc(DB,
    Vertex_Functor, Vertex_Id_Fld,
    Load_Vertex, Resolve_Destinations,
    V0, V,
    Stack0, Stack, Index0, Index, Scc_L0, Scc_L) :-

   must_be(nonneg, Index0),
   
   named_args_unify(V0,
                    [tarjan_index, tarjan_lowlink, Vertex_Id_Fld],
                    [Index0, Index0, V_Id]),
   debug(tarjan, 'Put vertex ~p into DB', [V_Id]),
   db_put_object(DB, V0, V1),
              
   succ(Index0, Index1),
   Stack1 = [V_Id|Stack0],
   (   debugging(tarjan)
   ->  length(Stack1, Stack1_Size),
       debug(tarjan, 'Stack size is ~d now', [Stack1_Size])
   ;   true   ),

   % Load all successors
   once(call(Resolve_Destinations, V1, W_Id_Lst)),
   (   debugging(tarjan)
   ->  length(W_Id_Lst, W_Id_Lst_Size),
       debug(tarjan, '~p is resloved to ~d destinations',
             [V_Id, W_Id_Lst_Size])
   ;   true ),

   scc_down(DB,
            Vertex_Functor, Vertex_Id_Fld,
            Load_Vertex, Resolve_Destinations,
            V1, V, W_Id_Lst,
            Stack1, Stack2, Index1, Index,
            Scc_L0, Scc_L1),

   named_args_unify(V,
                    [tarjan_index, tarjan_lowlink],
                    [V_Index, V_Lowlink]),
   
   (  V_Index =:= V_Lowlink
   ->
      % V it is a root node of an scc subtree
      scc_up(Stack2, Stack, [], Scc0, V_Id),
      sort(Scc0, Scc),
      debug(tarjan, 'Put ~p as SCC', [Scc]),
      Scc_L = [Scc|Scc_L1]
   ;
      Scc_L = Scc_L1,
      Stack = Stack2
   ).


scc_down(_, _, _, _, _, V, V, [],
         Stack, Stack, Index, Index, Scc_L, Scc_L) :-
   !.

scc_down(DB,
         Vertex_Functor, Vertex_Id_Fld,
         Load_Vertex, Resolve_Destinations,
         V0, V, [W_Id|W_Id_T],
         Stack0, Stack, Index0, Index, Scc_L0, Scc_L) :-

   obj_field(V0, tarjan_lowlink, V0_Lowlink),

   (  named_args_unify(DB, Vertex_Functor, 
                   [Vertex_Id_Fld], [W_Id], W0)
   ->
      % Dst_Page is loaded already
      W = W0,
      (   
          memberchk(W_Id, Stack0)
      ->
          % successor is in the current SCC
          obj_field(W, tarjan_index, W_Index),
          V1_Lowlink is min(V0_Lowlink, W_Index),
          obj_rewrite(V0, [tarjan_lowlink], _,  [V1_Lowlink], V1),
          db_put_object(DB, _, V1, V2, replaced),
          (   debugging(tarjan)
          ->  obj_field(V2, Vertex_Id_Fld, V2_Id_),
              debug(tarjan,
                    '~p already in SCC, set ~p lowlink to ~d',
                    [W_Id, V2_Id_, V1_Lowlink])
          ;   true )
      ;
          V2 = V0,
          debug(tarjan, '~p is loaded already', [W_Id])
      ),
      Stack1 = Stack0, Index1 = Index0, Scc_L1 = Scc_L0
   ;  
      % The vertex W0 has not yet been visited
      once(call(Load_Vertex, W_Id, W0)),
      debug(tarjan, 'New vertex ~p is loaded', [W_Id]),
      scc(DB,
          Vertex_Functor, Vertex_Id_Fld,
          Load_Vertex, Resolve_Destinations,
          W0, W, 
          Stack0, Stack1, Index0, Index1, Scc_L0, Scc_L1),

      obj_field(W, tarjan_lowlink, W_Lowlink),
      V1_Lowlink is min(V0_Lowlink, W_Lowlink),
      obj_rewrite(V0, [tarjan_lowlink], _,  [V1_Lowlink], V1),
      db_put_object(DB, _, V1, V2, replaced),
      (   debugging(tarjan)
      ->  obj_field(V1, Vertex_Id_Fld, V1_Id_),
          debug(tarjan,
                'Set ~p lowlink to ~d', [V1_Id_, V1_Lowlink])
      ;   true )
   ),
   scc_down(DB,
            Vertex_Functor, Vertex_Id_Fld,
            Load_Vertex, Resolve_Destinations,
            V2, V, W_Id_T,
            Stack1, Stack, Index1, Index, Scc_L1, Scc_L).

scc_up([W|Stack], Stack, Scc, [W|Scc], W) :- !.

scc_up([W|Stack0], Stack, Scc0, [W|Scc], V) :-

   scc_up(Stack0, Stack, Scc0, Scc, V).

