:- begin_tests(ur_subarrays).
:- use_module(u(ur_subarrays)).

test(list_subarray1, Y==[1,2,3,4]) :-
   list_subarray([1,2,3, 4], X), 
   list_subarray(Y, X).

test(list_subarray2, Y==[]) :-
   list_subarray([], X), 
   list_subarray(Y, X).

test(sa_length1, N1 == 0) :-
   list_subarray([], SA1),
   sa_length(SA1, N1),
   assertion(N1 == 0).

test(sa_length2, N1 == 2) :-
   list_subarray([a, b], SA1),
   sa_length(SA1, N1),
   assertion(N1 == 2).

test(sa_length3) :-
   list_subarray([a, b, c], SA1),
   sa_nth1(2, SA1, _, SA2),
   sa_length(SA2, N2),
   assertion(N2 == 2),
   sa_nth1(2, SA2, _, SA3),
   sa_length(SA3, N3),
   assertion(N3 == 1),
   sa_nth1(1, SA3, _, SA4),
   sa_length(SA4, N4),
   assertion(N4 == 0).

test(sa_nth1_1, fail) :-
   list_subarray([], SA),
   sa_nth1(1, SA, _, _).

test(sa_nth1_2) :-
   list_subarray([a], SA),
   sa_nth1(1, SA, El, RestSA),
   assertion(El == a),
   list_subarray(Rest, RestSA),
   assertion(Rest == []).

test(sa_nth1_3, error(type_error(_, _))) :-
   list_subarray([a], SA),
   sa_nth1(0, SA, _, _).

test(sa_nth1_4, fail) :-
   list_subarray([a], SA),
   sa_nth1(2, SA, _, _).

test(sa_nth1_5, Ps == [p(a, [b,c]), p(b, [a,c]), p(c, [a,b])]) :-
   list_subarray([a, b, c], SA),
   findall( p(L, R), 
            ( between(1, 3, I), 
              sa_nth1(I, SA, L, R0),
              list_subarray(R, R0) ),
            Ps).

test(sa_nth0_5, Ps == [p(a, [b,c]), p(b, [a,c]), p(c, [a,b])]) :-
   list_subarray([a, b, c], SA),
   findall( p(L, R), 
            ( between(0, 2, I), 
              sa_nth0(I, SA, L, R0),
              list_subarray(R, R0) ),
            Ps).

test(sa_nth1_6) :-
   list_subarray([a, b, c], SA),
   sa_nth1(2, SA, _, R0),
   sa_nth1(2, R0, El, R1),
   assertion(El == c),
   list_subarray(R, R1),
   assertion(R == [a]).

:- end_tests(ur_subarrays).
