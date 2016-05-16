:- begin_tests(massoc).
:- use_module(u(massoc)).

test(empty1, fail) :-
   empty_massoc(MA),
   get_massoc(1, MA, _, _, _).

test(empty2, fail) :-
   empty_massoc(MA),
   get_massoc(1, MA, _).

test(empty3, fail) :-
   empty_massoc(MA),
   del_massoc(1, MA, _, _).

test(get3_1, OldVal == a) :-
   massoc1(MAssoc),
   get_massoc(1, MAssoc, OldVal).

test(get3_2, Vals == [a, b, a]) :-
   massoc1(MAssoc),
   findall(Val, get_massoc(3, MAssoc, Val), Vals).

test(get3_3, fail) :-
   massoc1(MA),
   get_massoc(4, MA, _).

test(get5_1, [OldVal, NewVal] == [a, c]) :-
   massoc1(MAssoc),
   findall(OV-MA, get_massoc(1, MAssoc, OV, MA, c), [OldVal-MA1]),
   get_massoc(1, MA1, NewVal).

test(get5_2, Vals == [a, a, c]) :-
   massoc1(MAssoc),
   findall(MA, get_massoc(3, MAssoc, b, MA, c), [MA1]),
   findall(Val, get_massoc(3, MA1, Val), Vals0),
   msort(Vals0, Vals).

test(get5_3, fail) :-
   massoc1(MA),
   get_massoc(4, MA, _, _, _).

test(put1, C == c) :-
   massoc1(MA),
   put_massoc(4, MA, c, MA1),
   get_massoc(4, MA1, C).

test(put2, Vals == [a, a, b]) :-
   massoc1(MA),
   put_massoc(2, MA, a, MA1),
   findall(Val, get_massoc(2, MA1, Val), Vals0),
   msort(Vals0, Vals).

test(del1, DelVal == a) :-
   massoc1(MA),
   del_massoc(1, MA, DelVal, MA1),
   \+ get_massoc(1, MA1, _).

test(del2, Cnt == 1) :-
   massoc1(MA),
   aggregate_all(count,
                 ( del_massoc(3, MA, b, MA1),
                   \+ get_massoc(3, MA1, b)
                 ),
                 Cnt).

test(del3, DelVals == [a, b, a]) :-
   massoc1(MA),
   findall(DelVal, del_massoc(3, MA, DelVal, _), DelVals).

test(del4, fail) :-
   massoc1(MA),
   del_massoc(4, MA, _,  _).

test(min_massoc, KVs == [1-a]) :-
   massoc1(MA),
   findall(K-V, min_massoc(MA, K, V), KVs).

empty_massoc(MA) :-
   list_to_assoc([], MA).

massoc1(MAssoc) :-
   list_to_assoc([1-[a], 2-[a, b], 3-[a, b, a]], MAssoc).   

:- end_tests(massoc).