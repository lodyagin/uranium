%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009, 2011  Sergei Lodyagin
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

%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
% 
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%  ---------------------------------------------------------------
%
%  Description      : Data abstraction mechanismes.
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Apr 6 2009
%

:- module(sl_lists,
	  [corteging/4,  %+Functor, ?List1, ?List2, ?List3
           list_head/4,
           mapkeys/3,
           num_diff_list/2,
           index_list/4,
           extract_by_key_order/3,
           select_value/4,
           sort_linked/2,
           swap_keyed_list/2,
           switch_by_value/4,
           transpose_list_matrix/2,
           check_option_list/2,
           count_el/3,
           trim_list/4,
           decode_prop_list/3,
           convert_prop_list/3, % +Prop_List, +Functor, -List2
           prop_list_replace/4, % +In, -Out, +From, +To 
           weak_maplist/3,
           write_delimited/3     % +Write_Pred, +Delimiter, +List
]).

:- use_module(logging/logging).

:- module_transparent switch_by_value/4, weak_maplist/3.

corteging(_, [], [], []) :- !.

corteging(Functor, [A1|T1], [A2|T2], [Res|T_Res]) :-

  Res =.. [Functor, A1, A2],
  corteging(Functor, T1, T2, T_Res).

% [v, b, c, u], [_, _, 4, 2], [a, b, c, d] ->
% [c(v, _G5407563, a), c(b, _G5407671, b), c(c, 4, c), c(u, 2,
%  d)]
%corteging(Functor, LL, Res_List) :-

%  findall(H, member([H|_], LL), HL),

%  (   HL = []
%  ->  Res_List = []
%  ;   findall(T, member([_|T], LL), TL),
%      Res =.. [Functor|HL],
%      corteging(Functor, TL, T_Res),
%      Res_List = [Res|T_Res]
%  ).

transpose_list_matrix([], []) :- !.

transpose_list_matrix(L, []) :- flatten(L, []), !.

transpose_list_matrix(A, B) :-

   findall(H, member([H|_], A), A1),
   findall(T, member([_|T], A), A_),
   transpose_list_matrix(A_, B_),
   B = [A1|B_].

%  Head is the first M = min(N, length of List) elements of a List.
list_head(List, N, Head, M) :- list_head2(List, N, Head, M, 0).

list_head2(_, 0, [], M, M) :- !.

list_head2([], N, [], M, M) :- N >= 0, !.

list_head2([El|List], N, [El|Head], M, M_In) :- 
	N > 0,
	succ(N1, N),
	succ(M_In, M_In1),
	list_head2(List, N1, Head, M, M_In1).

%  mapkeys
mapkeys(_, [], []) :- !.

mapkeys(Pred, [Key1 - Val | Tail1], [Key2 - Val | Tail2]) :-
    call(Pred, Key1, Key2),
    mapkeys(Pred, Tail1, Tail2).

%  Return elements of keyed list in order of keys, defined by
%  another list, i.e., extract_by_key_order([1, 5, 2], [2 - abc,
%  1 - ddd, 5 - 4], [1 - ddd, 5 - 4, 2 - abc])
extract_by_key_order([], [], []) :- !.

extract_by_key_order(Order, Source, Destination) :-
    index_list(Order, Order_I, 1, succ),
    swap_keyed_list(Order_I, Order_SI),
    sort(Order_SI, Order_SSI),
    predsort(key_order_compare(Order_SSI), Source, Destination).

key_order_compare(Order_SSI, Ord, AI - _, BI - _) :-
    ord_memberchk(AI - K1, Order_SSI),
    ord_memberchk(BI - K2, Order_SSI),
    compare(Ord, K1, K2).

%  return difference between elements for numeric lists, i.e.
%  In_List - [10, 20, 15, 16, 33]
%  Diff_List - [10, -5, 1, 17]

num_diff_list([], []) :- !.

num_diff_list([_], []) :- !.

num_diff_list(In_List, Diff_List) :-
    In_List = [A, B | Tail],
    C is B - A,
    Diff_List = [C | Diff_Tail],
    num_diff_list([B | Tail], Diff_Tail).

%  Add indexes to the list from Start in order Succ_Pred(Start, Next) ...
index_list([], [], _, _) :- !.

index_list([A | Tail], [Start - A | I_Tail], Start, Succ_Pred) :-
    call(Succ_Pred, Start, I1),
    index_list(Tail, I_Tail, I1, Succ_Pred).

%  
swap_keyed_list([], []) :- !.

swap_keyed_list([A - B | T1], [B - A | T2]) :-
    swap_keyed_list(T1, T2).

%
% Выбор значения из списка на основе списка селекторов
%
select_value(Selector, Selector_List, Value_List, Value) :-
    nth1(Index, Selector_List, Selector),
    nth1(Index, Value_List, Value), !.

%
% Оператор case (switch)
%
switch_by_value(Selector, Selector_List, Goal_List, Default_Goal) :-
    nth1(Index, Selector_List, Selector) 
    ->
    nth1(Index, Goal_List, Goal),
    logged(Goal)
    ;
    Default_Goal.

%
% Проверка списка опций
% check_option_list(+Option_List, +Legal_Values)
% Проверяет, что все опции соответствуют Lagal_Values (add: и нет повторений)
%
check_option_list(Option_List, Legal_Values) :-
    maplist(check_option1(Legal_Values), Option_List)
    ->
    true
    ;
    write_log(['illegal option list -', Option_List]), fail.

check_option1(Legal_Values, Option) :-
    memberchk(Option, Legal_Values).

%
% Подсчет количества вхождений заданного элемента в список
%

count_el(List, Element, Count) :-
   count_el2(List, Element, 0, Count).

count_el2([], _, Count, Count).

count_el2([H | T], El, Already_Count, Count) :-
   (H = El  -> Count1 is Already_Count + 1; Count1 = Already_Count),
   count_el2(T, El, Count1, Count).


trim_list(_, [], [], _) :- !.

trim_list(left, [H|T], [H|T], Pred) :- \+ call(Pred, H), !.

trim_list(left, [_|T], Trimmed, Pred) :-
    trim_list(left, T, Trimmed, Pred).

trim_list(right, Untrimmed, Trimmed, Pred) :-
    nonvar(Untrimmed),
    reverse(Untrimmed, List1),
    trim_list(left, List1, List2, Pred),
    reverse(List2, Trimmed).

trim_list(both, Untrimmed, Trimmed, Pred) :-
    trim_list(left, Untrimmed, L, Pred),
    trim_list(right, L, Trimmed, Pred).


decode_prop_list(Prop_List, Field_Names, Field_Values) :-
  maplist(=.. , Prop_List, L1),
  maplist(nth0(0), L1, Field_Names),
  maplist(nth0(1), L1, Field_Values).

convert_prop_list(Prop_List, Functor, New_List) :-

    maplist(=.., Prop_List, Prop1),
    maplist(append([Functor]), Prop1, Prop2),
    maplist(=.., New_List, Prop2), !.

prop_list_replace([], [], _, _) :- !.

    
prop_list_replace([Prop_In|Tail_In],
                  [Prop_Out|Tail_Out],
                  From, To)
:-

        Prop_In =.. [In|In_Tail],
        (  nth0(Idx, From, In)
        -> nth0(Idx, To, Out),
           Prop_Out =.. [Out|In_Tail]
        ;  Prop_In = Prop_Out
        ),
        prop_list_replace(Tail_In, Tail_Out, From, To).
    

%
% weak_maplist(:Pred, ?List1, ?List2)
%
% The same as standard maplist but if Pred fails don't
% unify list elements.
%

weak_maplist(_, [], []) :- !.

weak_maplist(Pred, [Head1|Tail1], [Head2|Tail2]) :-

  ignore(call(Pred, Head1, Head2)),
  weak_maplist(Pred, Tail1, Tail2).

  
write_delimited(Write_Pred, Delimiter, List) :-

  List = [Head|Tail] ->

  call(Write_Pred, Head),
  maplist(write_delimited2(Write_Pred, Delimiter), Tail)

  ; true.

write_delimited2(Write_Pred, Delimiter, El) :-

  call(Write_Pred, Delimiter),
  call(Write_Pred, El).


% LU is a list of lists of the same size Sort the first list and
% change the order of elements in others lists like all elements
% with the same index are linked in all lists.
sort_linked(LU, LS) :-

   transpose_list_matrix(LU, LU_Tr),
   findall(H-T, member([H|T], LU_Tr), Keyed_LU_Tr),
   keysort(Keyed_LU_Tr, Keyed_LS_Tr),
   findall([H|T], member(H-T, Keyed_LS_Tr), LS_Tr),
   transpose_list_matrix(LS_Tr, LS).
   
