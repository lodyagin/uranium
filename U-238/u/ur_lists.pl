%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
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

:- module(ur_lists,
	  [
           check_option_list/2,
           common_head/3,     % +List1, +List2, ?Head
           common_head_rev/3, % +List1, +List2, ?Head
           convert_prop_list/3, % +Prop_List, +Functor, -List2
           corteging/4,       % ?Functor, ?List1, ?List2, ?List3
           count_el/3,
           decode_prop_list/3,
           extract_by_key_order/3,
           gen_memberchk/3,   % +Op, ?Member, +List
           index_list/4,
           list_head/4,
           mapkeys/3,
           num_diff_list/2,
           pairs_replace_functor/3,
           prop_list_replace/4, % +In, -Out, +From, +To
           remove_options/3, % +List0, +Remove, -List
           replace_all_sublists/4,
           replace_tail/4, % Tail1, List1, Tail, List
           select_value/4, % +Selector, +Selectors, % ?Values,
                           % ?Value (det)

           select_value/6, % ?Selector, +Selectors, %
                           % -Selectors_Rest, ?Values, %
                           % ?Values_Rest, ?Value (nondet)
           sort_linked/2,
           swap_keyed_list/2,
           switch_by_value/4,
           transpose_list_matrix/2,
           trim_list/4,
           weak_maplist/3,
           weak_maplist/4,
           skip_maplist/4,       % :Pred, ?L1, ?L2, ?L3
           skip_maplist/5,       % :Pred, ?L1, ?L2, ?L3, ?L4
           write_delimited/3     % +Write_Pred, +Delimiter, +List
]).

/** <module> Auxiliary list operations
*/

:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(u(logging)).

:- meta_predicate gen_memberchk(2, ?, ?).
:- meta_predicate write_delimited(1, +, +).

:- module_transparent switch_by_value/4, weak_maplist/3.


%% common_head(+List1, +List2, ?Head)
%
%  True if Head is a common prefix of List1 and List2.

common_head(List1, List2, Head) :-
   common_head(List1, List2, [], Head1),
   reverse(Head1, Head).

common_head_rev(List1, List2, Head) :-
   common_head(List1, List2, [], Head).

common_head([], _, L, L) :- !.
common_head(_, [], L, L) :- !.
common_head([A|AT], [A|BT], L0, L) :- !,
   common_head(AT, BT, [A|L0], L).
common_head(_, _, L, L).


%% corteging(?Functor, ?List1, ?List2, ?Cortege)
%
%  Cortege is a list of terms with functor Functor, List1
%  elements as the first args and List2 as the second. For
%  example:
%
%  ==
%  corteging(+, [1, 5], [4, 9], X).
%
%  X = [1+4, 5+9]
%  ==

corteging(_, [], [], []) :- !.

corteging(Functor, [A1|T1], [A2|T2], [Res|T_Res]) :-

  Res =.. [Functor, A1, A2],
  corteging(Functor, T1, T2, T_Res).


%% gen_memberchk(:Op, ?Member, +List)
%
%  It is like memberchk but use a user defined Op/2 for compare
%  or unify Member with elements of List.

gen_memberchk(Op, Member, List) :-

   must_be(nonvar, List),
   must_be(callable, Op),

   gen_memberchk_int(Op, Member, List).

gen_memberchk_int(_, _, []) :- fail.

gen_memberchk_int(Op, Member, [El|T]) :-

   (  call(Op, Member, El)
   -> true
   ;  gen_memberchk(Op, Member, T)
   ).



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

% replace the functor in pairs
pairs_replace_functor(_, [], []) :- !.

pairs_replace_functor(New_Functor, [El1|T1], [El2|T2]) :-

   El1 =.. [_, Key, Value],
   El2 =.. [New_Functor, Key, Value],
   pairs_replace_functor(New_Functor, T1, T2).

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


% remove_options(+List0, +Remove, -List)
%

remove_options(List, [], List) :- !.

remove_options(List0, [Option0|Remove], List) :-

   (  atom(Option0)
   -> functor(Option, Option0, 1)
   ;  Option = Option0
   ),
   select_option_req(Option, List0, List1),
   remove_options(List1, Remove, List).

%% replace_all_sublists(+From, +List0, +To, ?List1) is det.
%
% List1 is List0 with all sublists From replaced to To.

replace_all_sublists(From, List0, To, List1) :-

%   must_be(list, From),
%   must_be(list, List0),
   From \= [],
   phrase(replace_all_sublists_dcg(From, To, List1, []), List0).

replace_all_sublists_dcg(From, To, List0, List) -->
   From, !,
   { append(To, List1, List0) },
   replace_all_sublists_dcg(From, To, List1, List).

replace_all_sublists_dcg(From, To, [C|List0], List) -->
   [C], !,
   replace_all_sublists_dcg(From, To, List0, List).
   
replace_all_sublists_dcg(_, _, List, List) -->
   [].

%% replace_tail(Tail1, List1, Tail, List) is nondet.
%  List1 = [head |Tail1], List = [head |Tail]
%  Attention! Gives infinite number of solutions, use with cut.
replace_tail(Tail1, Tail1, Tail, Tail) :- acyclic_term(Tail1).
replace_tail(Tail0, [X|Tail1], T0, [X|T1]) :-
   replace_tail(Tail0, Tail1, T0, T1).

select_option_req(Option, List0, List) :-

   copy_term(Option, Option1),
   (  select_option(Option1, List0, List1)
   -> select_option_req(Option, List1, List)
   ;  List = List0
   ).


% select_value(+Selector, +Selector_List, +Value_List, -Value) :-
% det
%
% Выбор значения из списка на основе списка селекторов
%
select_value(Selector, Selector_List, Value_List, Value) :-

   nonvar(Selector),
   select_value(Selector, Selector_List, _,
                Value_List, _, Value),
   !.

% select_value(?Selector, +Selectors,   -Selectors_Rest,
%              ?Values,   ?Values_Rest, ?Value) :-
% nondet
%
% The same but return new lists without Selector and Value
% Always complete Values argument if it is var or a partial list
%


select_value(_, [],  _, _, _, _) :- fail.

select_value(Selector, [Selector|SR], SR,
             [Value|VR], VR, Value) :-

   (   nonvar(VR) -> true
   ;   same_length(SR, VR) % complete a partial list
   ).

select_value(Selector, [SH|ST], [SH|SR],
             [VH|VT], [VH|VR], Value) :-

   select_value(Selector, ST, SR, VT, VR, Value).

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


trim_list(_, _, [], []) :- !.

trim_list(left, U, [H|T], [H|T]) :-

   \+ memberchk(H, U), !.

trim_list(left, U, [_|T], Trimmed) :-

   trim_list(left, U, T, Trimmed).

trim_list(right, U, Untrimmed, Trimmed) :-
    nonvar(Untrimmed),
    reverse(Untrimmed, List1),
    trim_list(left, U, List1, List2),
    reverse(List2, Trimmed).

trim_list(both, U, Untrimmed, Trimmed) :-
    trim_list(left, U, Untrimmed, L),
    trim_list(right, U, L, Trimmed).


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


:- meta_predicate weak_maplist(2, ?, ?).

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


:- meta_predicate weak_maplist(3, ?, ?, ?).

%
% weak_maplist(:Pred, ?List1, ?List2, ?List3)
%
% The same as standard maplist but if Pred fails don't
% unify list elements.
%
weak_maplist(_, [], [], []) :- !.
weak_maplist(Pred, [Head1|Tail1], [Head2|Tail2], [Head3|Tail3]) :-
  ignore(call(Pred, Head1, Head2, Head3)),
  weak_maplist(Pred, Tail1, Tail2, Tail3).


:- meta_predicate skip_maplist(3, ?, ?, ?).
:- meta_predicate skip_maplist(4, ?, ?, ?, ?).

%% skip_maplist(:Pred, ?List1, ?List2, ?List3)
%
% The same as standard maplist but if Pred fails skip this list element.
%
skip_maplist(_, [], [], []) :- !.
skip_maplist(Pred, L1, L2, L3) :-
  (  L1 = [H1|T1], 
     L2 = [H2|T2],
     L3 = [H3|T3],
     call(Pred, H1, H2, H3)
  -> skip_maplist(Pred, T1, T2, T3)
  ;  skip_one_element(L1, M1),
     skip_one_element(L2, M2),
     skip_one_element(L3, M3),
     skip_maplist(Pred, M1, M2, M3)
  ).

%% skip_maplist(:Pred, ?List1, ?List2, ?List3, ?List4)
%
% The same as standard maplist but if Pred fails skip this list element.
%
skip_maplist(_, [], [], [], []) :- !.
skip_maplist(Pred, L1, L2, L3, L4) :-
  (  L1 = [H1|T1], 
     L2 = [H2|T2],
     L3 = [H3|T3],
     L4 = [H4|T4],
     call(Pred, H1, H2, H3, H4)
  -> skip_maplist(Pred, T1, T2, T3, T4)
  ;  skip_one_element(L1, M1),
     skip_one_element(L2, M2),
     skip_one_element(L3, M3),
     skip_one_element(L4, M4),
     skip_maplist(Pred, M1, M2, M3, M4)
  ).

skip_one_element(L, L) :- var(L), !.
skip_one_element([_|T], T).

write_delimited(_:atom(Atom), Delimiter, List) :- !,

   write_delimited_atom(List, Delimiter, _, Codes, []),
   atom_codes(Atom, Codes).

write_delimited(Write_Pred, Delimiter, List) :-

  List = [Head|Tail] ->

  call(Write_Pred, Head),
  maplist(write_delimited2(Write_Pred, Delimiter), Tail)

  ; true.

write_delimited2(Write_Pred, Delimiter, El) :-

  call(Write_Pred, Delimiter),
  call(Write_Pred, El).

write_delimited_atom([], _, _, T, T) :- !.

write_delimited_atom([Head|Tail], Delimiter, First, Codes, CT) :-

   (  First = first
   -> Del = ''
   ;  Del = Delimiter
   ),

   format(codes(Codes, CT1), '~a~p', [Del, Head]),
   write_delimited_atom(Tail, Delimiter, tail, CT1, CT).



% LU is a list of lists of the same size. Sort the first list and
% change the order of elements in others lists like all elements
% with the same index are linked in all lists.
sort_linked(LU, LS) :-

   transpose_list_matrix(LU, LU_Tr),
   findall(H-T, member([H|T], LU_Tr), Keyed_LU_Tr),
   keysort(Keyed_LU_Tr, Keyed_LS_Tr),
   findall([H|T], member(H-T, Keyed_LS_Tr), LS_Tr),
   transpose_list_matrix(LS_Tr, LS).

