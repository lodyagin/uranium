%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

%  -*-coding: mule-utf-8-unix; fill-column: 58-*-
%
%  Copyright (C) 2009, 2011 Kogorta
%
%  Description      : Проверка того, что операции
%                     выполнились.
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Apr 6 2009
%

:- module(check_result,
          [check/1,
           check/2
          ]).

:- use_module(library(ur_objects)).
:- use_module(library(ur_lists)).
:- use_module(library(fixed)).

% arithmetic evaluations

a_eval(Number, Number) :-

   number(Number), !.

a_eval(Object/Field, Result) :-

   compound(Object),
   functor(Object, Functor, _),
   atom_concat(_, '_v', Functor),
   atom(Field), !,
   eval_obj_expr(Object/Field, Something),
   (  atom(Something)
   -> fixed_from_term(Result, Something)
   ;  Result = Something
   ).

a_eval(Expr, Result) :-

   compound(Expr),
   Expr =.. [Fun|Args],
   maplist(a_eval, Args, Res_List),
   Expr2 =.. [Fun|Res_List],
   Result is Expr2.

check(Left = Right) :-

        eval_obj_expr(Left, Result),
        eval_obj_expr(Right, Value),
        ground(Result),
        ground(Value),
        (
         Result = Value -> true
        ;
         write_log(['check failed: showld be', Value, 'got', Result],
                   [lf(1, before), lf(2)]),
         !, fail %b_setval(check_result, failed)
        ).

check(Left =:= Right) :-

        a_eval(Left, Result),
        a_eval(Right, Value),
        (
         Result =:= Value -> true
        ;
         write_log(['the arithmetic check failed: showld be', Value, 'got', Result],
                   [lf(1, before), lf(2)]),
         !, fail %b_setval(check_result, failed)
        ).

check(List) :-

  is_list(List), !,
  maplist(check_one_from_list, List).

check_one_from_list(Element) :-

  write_log(['->check:', Element], [lf(1, before)]),
  (   check(Element)
  ->  write_log(' |ok|', [module(no), lf(1)])
  ;   !, fail
  ).

check(Expr, Attr_List) :-
  is_list(Attr_List),
  eval_obj_expr(Expr, Element),
  nonvar(Element),
  functor(Element, Functor, _),
  concat(_, '_v', Functor),
  maplist(check_one(Element), Attr_List).

check_one(Element, Attr) :-
  Attr =.. [Attr_Name, Attr_Val],
  obj_field(Element, Attr_Name, Obj_Val),
  ground(Obj_Val),
  ground(Attr_Val),
  equal_field_value(Obj_Val, Attr_Val).

equal_field_value(Val, Val).

                    
  

  