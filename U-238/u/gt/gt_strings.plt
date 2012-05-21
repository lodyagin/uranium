:- use_module(library(clpfd)).

:- begin_tests(random_string).
:- use_module(u(gt/gt_strings)).

test(all_different_full) :-

  First = 0'a,  %'
  Last = 0'z,   %'
  Length is Last - First + 1,
  numlist(First, Last, Etalon),
  random_string(all_different_pat(First..Last), [length(Length)],
		Codes),
  perm_check(Codes, Etalon).
  % Codes must be always a permutation of the Etalon

test(empty_atom) :-

  findall(A, (between(1, 100, _),
	      random_string([empty], atom(A))),
	  AL),
  sort(AL, ['']).

test(one_length_one_char) :-

  findall(A, (between(1, 100, _),
	      random_string([length(5), range(0'a)], atom(A))), %'
	      AL),
  sort(AL, ['aaaaa']).

test(regex1) :-

   random_string([regex("a(b|c)*d"), length(100)], atom(Str)),
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

% add test on only one lenth option

% on all length options

:- end_tests(random_string).

% deterministic permutation check

perm_check(L1, L2) :-

  permutation(L1, L2), !.

% Some patterns

all_different_pat(Drep, Str) :-

  gt_strings:range_pattern(Drep, Str),
  all_different(Str).






