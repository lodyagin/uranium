%  -*-coding: mule-utf-8-unix; fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.

%  Copyright (C) 2011  Sergei Lodyagin
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
%  --------------------------------------------------------------
% 

:- module(ur_terms,
          [
           arg_bac/3,
           arg_bca/3,
           chain_call/4,       % :Chain, +Weak, ?In, ?Out
           replace_subterms/3, % :Replacer, @Term0, -Term
           seeded_rep/5,       % :GenPred, :AccPred, +Count, 
                               % +Seed0, -Seed
           setarg_tnv/3,
           weak_arg_bac/3
          ]).

:- use_module(library(error)).

% like arg but with different orders
arg_bac(B, A, C) :- arg(A, B, C).
arg_bca(B, C, A) :- arg(A, B, C).

% like arg_bac but fail on unbound A element
weak_arg_bac(B, A, C) :- ground(A), arg(A, B, C).


setarg_tnv(Term, Number, Value) :-
   setarg(Number, Term, Value).

:- meta_predicate replace_subterms(2, +, -).

%% replace_subterms(:Replacer, @Term0, -Term) is nondet.
%
% For Term0 and all subterms it calls Replacer/2. If the Replacer
% succeeds replaces the subterm with the second argument of the
% Replacer. Outer terms have a chance to be replaced earlier
% than subterms (it goes down recursively). Det/nondet depends
% on the Replacer.
%
% @param Replacer Replacer(@From, -To). Must not alter From. 
%                 Also it is possible to have Replacer as a list.
%                 In this case it will substitute Replacer
%                 with chain_call(Replacer, skip).
%
replace_subterms(_:[], Term, Term) :- !.
replace_subterms(M:Replacers, Term0, Term) :-
   Replacers = [_|_], !,
   replace_subterms(chain_call(M:Replacers, skip), Term0, Term).
replace_subterms(Replacer, Term0, Term) :-
   var(Term0), !,
   ignore(call(Replacer, _, Term)). % NB doesn't touch Term0
replace_subterms(Replacer, Term0, Term) :-
   (  call(Replacer, Term0, Term1) -> true
   ;  Term1 = Term0 % No replace, go further down
   ),
   (  compound(Term1)
   -> Term1 =.. [Head1|Tail1],
      maplist(replace_subterms(Replacer), Tail1, Tail),
      Term =.. [Head1|Tail]  % never replace a head
   ;  Term = Term1
   ).

:- meta_predicate seeded_rep(3, 1, +, +, -).

%% seeded_rep(:GenPred, :AccPred, +Count, +Seed0, -Seed) is det.
%
% Calls GenPred(Seed0, Seed1, X) and then AccPred(X) Count times.
% Pass Seed1 as Seed0 to the next call. Unifies Seed 
% with Seed1 of the last GenPred called.
%
seeded_rep(_, _, 0, Seed, Seed) :- !.
seeded_rep(GenPred, AccPred, Count, Seed0, Seed) :-
   (  integer(Count), Count > 0 -> true
   ;  must_be(nonneg, Count) 
   ),
   (  call(GenPred, Seed0, Seed1, X) -> true
   ;  throw(error(pred_fails(GenPred), contex(seeded_rep/5, _)))
   ),
   (  call(AccPred, X) -> true
   ;  throw(error(pred_fails(AccPred), contex(seeded_rep/5, _)))
   ),
   succ(Count1, Count),
   seeded_rep(GenPred, AccPred, Count1, Seed1, Seed).

:- meta_predicate chain_call(:, +, ?, ?).

%% chain_call(:Chain, +Weak, ?In, ?Out) is nondet.
%
% Chain must be a list of Pred/2. Calls all Pred and passes In
% to the first arg of the first Pred, the second arg of Pred to
% the first arg of the next Pred in chain and the second arg
% of the last Pred unifies with Out. Det/nondet depends on preds
% in the chain.
%
% @param Weak fail - fail the chain if at least one pred in the Chain
%             fails, skip - skip the failing preds from the chain.
%
chain_call(_:[], _, A, A) :- !.
chain_call(M0:[Pred0|ChainTail], Weak, In, Out) :-
   (  Pred0 = _:_
   -> Pred = Pred0 
   ;  Pred = M0:Pred0
   ),
   (  Weak == fail
   -> call(Pred, In, Pass)
   ;  Weak == skip
   -> (  call(Pred, In, Pass) -> true ; Pass = In )
   ;  must_be(oneof([fail, skip]), Weak)
   ),
   chain_call(M0:ChainTail, Weak, Pass, Out).
   
