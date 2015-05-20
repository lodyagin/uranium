:- begin_tests(ur_terms).
:- use_module(u(ur_terms)).
:- use_module(library(clpfd)).

test(chain_call1, A==B) :-
   chain_call([], fail, A, B).

test(chain_call2, fail) :-
   chain_call([fail_pred], fail, _, _).

test(chain_call3, A==B) :-
   chain_call([fail_pred], skip, A, B).

test(chain_call4, 7==B) :-
   chain_call([succ, succ], fail, 5, B).

test(chain_call5, 7==B) :-
   chain_call([succ, fail_pred, succ], skip, 5, B).

test(chain_call6, C==cba) :-
   chain_call([ur_atoms:smth_codes, reverse], fail, "abc", B),
   atom_codes(C, B).

test(replace_subterms1, [A, B]=@=[_,4]) :-
   replace_subterms(var_replacer, A, B).

test(replace_subterms2, T==p(1, 2, p(3), 1..8)) :-
   replace_subterms(calculator, p(1, 2, p(1 + 2), 1..3*8/3), T).

test(replace_subterms3, T==v(1, 2, v(3), 1..8)) :-
   replace_subterms([pv,calculator], p(1, 2, p(1 + 2), 1..3*8/3), T).

test(replace_subterms4, T==[1, 2, 3, 1..8]) :-
   replace_subterms(calculator, [1, 2, 1 + 2, 1..3*8/3], T).


fail_pred(_, _) :- fail.

var_replacer(A, A) :- A=4.

calculator(Expr, Result) :- 
    catch(Result is Expr, _, Result = Expr).

pv(A, B) :-
  compound(A),
  A=..[p|T],
  B=..[v|T].

:- end_tests(ur_terms).
