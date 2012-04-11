:- begin_tests(ur_lists).
:- use_module(u(ur_lists)).

test(remove_options,
     [List == [b(2), a(2, x), 3]]
     ) :-

   remove_options([z = 3, b(2), a(2, x), 3, a= 1, z=_],
                  [z, a(_)],
                  List).

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

:- end_tests(ur_lists).
