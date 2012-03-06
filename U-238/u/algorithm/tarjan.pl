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

:- module(tarjan, [tarjan/4]).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(action/click_url)).
:- use_module(u(parser/html/html_page_parse)).

tarjan(Pages, Links, Url, Scc_List) :-

   load_page(Pages, Links, Url),
   scc(Pages, Links, Url, [], [], 0, _, [], Scc_List, _).


scc(Pages, Links, Src_Url,
    Stack0, Stack, Index0, Index, Scc_L0, Scc_L,
    V) :-

   V0 = vertex(Src_Url, Index0, Index0),

   Index1 is Index0 + 1,
   Stack1 = [V|Stack0],  %  push to stack

   % Get all destination urls
   findall(Dst_Url,
           (db_iterate(Links, http_request_url(Src_Url), Link),
            obj_field(Link, global_link_url, Dst_Url)),
           Dst_Url_List),

   scc_down(Pages, Links, Dst_Url_List,
            Stack1, Stack2, Index1, Index, Scc_L0, Scc_L1,
            V0, V),

   (  V = vertex(_, V_Index, V_Lowlink),
      V_Index =:= V_Lowlink
   ->
      % V it is a root node of an scc subtree
      scc_up(Stack2, Stack, [], Scc, V),
      Scc_L = [Scc|Scc_L1]
   ;
      Scc_L = Scc_L1
   ).


scc_down(_, _, [], Stack, Stack, Index, Index, Scc_L, Scc_L, V, V).

scc_down(Pages, Links, [Dst_Url|Dst_Url_T],
         Stack0, Stack, Index0, Index, Scc_L0, Scc_L,
         V0, V) :-

   V0 = vertex(Src_Url, V0_Index, V0_Lowlink),
   V1 = vertex(Src_Url, V0_Index, V1_Lowlink),
   
   (  \+ named_arg_unify(Pages,
                         http_request_url, Dst_Url, _)
   ->
      % found a new page, load and recurse
      load_page(Pages, Links, Dst_Url),
      scc(Pages, Links,
          Stack0, Stack1, Index0, Index1, Scc_L0, Scc_L1,
          vertex(Dst_Url, _, W_Lowlink)
         ),
      V1_Lowlink is min(V0_Lowlink, W_Lowlink)
   ;
      % Dst_Page is loaded already
      memberchk(vertex(Dst_Url, W_Index, _), Stack0)
   ->
      % successor is in the current SCC
      V1_Lowlink is min(V0_Lowlink, W_Index)
   ),
   scc_down(Pages, Links, Dst_Url_T,
            Stack1, Stack, Index1, Index, Scc_L1, Scc_L,
            V1, V).
          
scc_up([W|Stack], Stack, Scc, [W|Scc], W) :- !.

scc_up([W|Stack0], Stack, Scc0, [W|Scc], V) :-

   scc_up(Stack0, Stack, Scc0, Scc, V).


% load_page(+Pages_DB, +Links_DB, +Url)
% It is called iff Url is not in Pages

load_page(Pages_DB, Links_DB, Url) :-

   click_url(Url, Page),
   db_put_object(Pages_DB, Page, _),
   html_page_parse(Links_DB, Page, [link_v]).
