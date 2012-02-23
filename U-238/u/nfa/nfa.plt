:- begin_tests(nfa).
:- use_module(u(nfa/nfa)).

test(one_symbol_nfa_dfa,
     [dfa(States, Arcs, Initial, Final) == dfa([1, 2], [arc(1, 0'a, 2)], 1, [2])]
    ) :-

	one_symbol_nfa(NFA),
	nfa_dfa(NFA, dfa(States, Arcs, Initial, Final)).

test(dragon_book_nfa_dfa,
     [c(N_States, N_Arcs, N_Final) == c(5, 10, 1) ]
    ) :-

	dragon_book_nfa(NFA),
	nfa_dfa(NFA, DFA),
	DFA = dfa(States, Arcs, _, Final),
	length(States, N_States),
	length(Arcs, N_Arcs),
	length(Final, N_Final).


one_symbol_nfa(nfa([1, 2], [arc(1, 0'a, 2)], 1, [2])).

% NFA for (a|b)*abb
dragon_book_nfa(nfa(States, Arcs, 0, [10])) :-

	length(States, 11),
	numlist(0, 10, States),
	Arcs = [
		arc(0, -1, 1), arc(0, -1, 7),
		arc(1, -1, 2), arc(1, -1, 4),
		arc(2, 0'a, 3),
		arc(3, -1, 6),
		arc(4, 0'b, 5),
		arc(5, -1, 6),
		arc(6, -1, 1), arc(6, -1, 7),
		arc(7, 0'a, 8),
		arc(8, 0'b, 9),
		arc(9, 0'b, 10)
		].

:- end_tests(nfa).
