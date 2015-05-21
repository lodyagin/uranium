:- begin_tests(ur_lists).
:- use_module(u(ur_lists)).
:- use_module(u(util/lambda)).

test(remove_options1,
     [List == [b(2), a(2, x), 3]]
     ) :-

   remove_options([z = 3, b(2), a(2, x), 3, a= 1, a(2), z=_],
                  [z, a(_)],
                  List).

test(remove_options2,
     [List == [b(2), 3]]
     ) :-

   remove_options([z = 3, b(2), a(2, x), 3, a= 1, z=_],
                  [z, a(_), a(_, _)],
                  List).

test(replace_all_sublists1) :-

   replace_all_sublists([a], [a, b, c, a, d], [1], X1),
   assertion(X1 == [1, b, c, 1, d]),
   replace_all_sublists([a], [a, b, c, a, d], [1, 2], X2),
   assertion(X2 == [1, 2, b, c, 1, 2, d]),
   replace_all_sublists([a], [a, b, c, a, d], [], X3),
   assertion(X3 == [b, c, d]),
   replace_all_sublists([a, b], [a, b, c, a, d], [1], X4),
   assertion(X4 == [1, c, a, d]).

test(replace_all_sublists2, [fail]) :-

   replace_all_sublists([], [a, b, c, a, d], [1], _).
   
test(replace_tail1, [Y == Z]) :-
  replace_tail(X, X, Y, Z), !.

test(replace_tail2, [Z == [a,b,c|Y]]) :-
  replace_tail(X, [a,b,c|X], Y, Z), !.

test(replace_tail3, [Z == [a,b,c|Y]]) :-
  replace_tail([d,e], [a,b,c,d,e], Y, Z), !.

test(replace_tail4, fail) :-
  replace_tail([d], [a,b,c,d,e], _, _), !.

test(select_value1, [fail]) :-

   select_value(_, [], _, [], _, _).

test(select_value2,
     [Result ==  [1 - [[b, a, d, a], [2, 3, 4, 5]],
                  3 - [[a, b, d, a], [1, 2, 4, 5]],
                  5 - [[a, b, a, d], [1, 2, 3, 4]]]
     ]) :-

   findall(Val - [SR, VR],
           select_value(a, [a, b, a, d, a], SR,
                        [1, 2, 3, 4, 5], VR, Val),
           Result
          ).

test(select_value3, [fail]) :- 

   select_value(4, [a, b, a, d, a], [5, 4, 3, 2, 1], _).

test(select_value4) :- 

   select_value(b, [a, b, a, d, a], _, X, _, 4), !,
   assertion(X =@= [_, 4, _, _, _]).

test(select_value5) :- 

   select_value(b, [a, b, a, d, a], _, X, [1, 3, 4, 5], V), !,
   assertion(X == [1, V, 3, 4, 5]).

test(select_value6) :- 

   select_value(d, [a, b, a, d, a], _, [1|X], _, _), !,
   assertion(X =@= [_, _, _, _]).

test(select_value2_1, [Value == 5]) :-

   select_value(a, [a, b, a, d, a], [5, 4, 3, 2, 1], Value).

test(write_delimited1_atom, [A == 'a:b:c']) :-

   write_delimited(atom(A), ':', [a, b, c]).

test(skip_maplists_arity5, L == [2, 4]) :-
   skip_maplist(\A^B^C^D^(A=C, B=D), 
                [1, 2, 3, 4], 
                [1, 3, 2, 4], 
                [2, 1, 3, 4], 
                L).

test(list_subarray1, Y==[1,2,3,4]) :-
   list_subarray([1,2,3, 4], X), 
   list_subarray(Y, X).

:- end_tests(ur_lists).
