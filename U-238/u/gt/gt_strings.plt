:- use_module(library(clpfd)).

:- begin_tests(gt_strings).
:- use_module(u(gt/gt_strings)).

test(all_different_full) :-
  First = 0'a,  %'
  Last = 0'z,   %'
  Length is Last - First + 1,
  numlist(First, Last, Etalon),
  random_string([pattern(all_different_pat(First..Last)),
                 length(Length)], _,
		Codes),
  perm_check(Codes, Etalon).
  % Codes must be always a permutation of the Etalon

test(empty_atom) :-
  findall(A, (between(1, 100, _),
	      random_string([empty], _, atom(A))),
	  AL),
  sort(AL, ['']).

test(one_length_one_char) :-
  findall(A, (between(1, 100, _),
	      random_string([length(5), range(0'a)], _, atom(A))), %'
	      AL),
  sort(AL, ['aaaaa']).

test(regex1) :-
   random_string([regex("a(b|c)*d"), length(100)], _, atom(Str)), 
   atom_length(Str, N),
   assertion(N == 100),
   sub_atom(Str, 0, 1, _, X1),
   sub_atom(Str, 1, _, 1, X2),
   sub_atom(Str, _, 1, 0, X3),
   assertion(X1 == a),
   atom_codes(X2, C2),
   sort(C2, CC),
   assertion(CC == "bc"),
   assertion(X3 == d).

test(randsel, 
     [LL == [[3,2,1],[3,1,2],[2,2,1],
             [2,3,1],[2,1,1],[2,1,3],
             [2,1,2],[1,2,3],[1,2,1],
             [1,2,2],[1,3,2],[1,1,2]
            ]]
) :-
   N1 #>= 1, 
   N2 #>= 1, 
   length(L, 3), 
   global_cardinality(L, [1-N1, 2-N2, 3-_], []), 
   findall(L, randsel([], L, randgen:test_sequence1, 1, _), LL).

test(random_string_nondet, 
     [Atom == bca3bcb4bcc4bbb4bbc5bba5bab4bac5baa5aab4aac5aaa5abc5aba6abb6acc5aca6acb6cab4cac5caa5cbc5cba6cbb6ccc5cca6ccb6]
) :-
   findall([Str, S], 
           random_string([length(3), range(0'a..0'c), nondet, rand_state(0, S), 
                          generator(randgen:test_sequence1)], _,
                         atom(Str)), 
           L), 
   flatten(L, SL),
   concat_atom(SL, Atom).

% add test on only one lenth option

% on all length options

:- end_tests(gt_strings).

% deterministic permutation check

perm_check(L1, L2) :-

  permutation(L1, L2), !.

% Some patterns

all_different_pat(Drep, Str) :-

  gt_strings:range_pattern(Drep, Str),
  all_different(Str).







