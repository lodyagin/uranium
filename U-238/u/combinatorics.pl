:- module(combinatorics,
          [combination/3
          ]).

:- use_module(u(ur_lists)).

%% combination(L, R, P) is nondet.
%
% P is a combination of R elements from L.  
%
combination(L0, R, P) :-
  Ctx = context(combination/3, _),
  list_to_subarray(L0, L, Ctx),
  combination_int(L, R, P).

combination_int(L, R, P) :-
  R > 0, succ(R1, R),
  sa_nth1(1, L, X, Lt), % L = [X|Lt]
  (  % P1 is combination of length R – 1 without X
     combination_int(Lt, R1, P1), 
     sa_nth1(_, P, R1, P1)
  ;
     % P is combination of length R without X
     combination_int(Lt, R, P)
  ).
