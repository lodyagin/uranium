:- module(regex,
	  [regex_nfa/2,
	   regex_dfa/2
	  ]
	 ).

:- use_module(u(nfa/nfa)).

% nfa(States, Arcs, Initial, Final).
regex_nfa(Regex, NFA) :-

	phrase(regex(NFA), Regex, []).

regex_dfa(Regex, DFA) :-

	regex_nfa(Regex, NFA),
	nfa_dfa(NFA, DFA).

regex(NFA) -->
	(   expr2(NFA1), !,
	    {
	     NFA1 = nfa(States0, Arcs, Initial, Final),
	     sort(States0, States),
	     length(States, NStates),
	     numlist(1, NStates, States),
	     NFA = nfa(States, Arcs, Initial, Final)
	    }
	;   [],
	    { NFA = nfa([1], [], 1, [1]) }
	).

expr2(NFA) -->
	part(NFA1), "|", expr2(NFA2), !, { or_nfa(NFA1, NFA2, NFA) }
	| part(NFA).

part(NFA) -->
	left(NFA1), part(NFA2), !, { conc_nfa(NFA1, NFA2, NFA) }
	| left(NFA).

left(NFA) -->  mult(NFA), ! | solid(NFA) .

mult(NFA) --> solid(NFA1), "*", { mult_nfa(NFA1, NFA) }.

solid(NFA) --> "(", expr2(NFA), ")" | letter(NFA).

letter(nfa([Start, End],
	   [arc(Start, C, End)],
	   Start, [End])
      )
-->

	[C], { is_usual_symbol(C) }.

is_usual_symbol(C) :-

   \+ member(C, "()*|").


conc_nfa(nfa(States1, Arcs1, Initial1, [Final1]),
	 nfa(States2, Arcs2, Final1, [Final2]),
	 nfa(States3, Arcs3, Initial1, [Final2])
	) :-

	append(States1, States2, States3),
	append(Arcs1, Arcs2, Arcs3).

or_nfa(nfa(States1, Arcs1, Initial1, [Final1]),
       nfa(States2, Arcs2, Initial2, [Final2]),
       nfa(States3, Arcs3, Initial3, [Final3])
      ) :-

	append(States1, States2, States_),
	States3 = [Initial3,Final3|States_],
	append(Arcs1, Arcs2, Arcs_),
	Arcs3 = [arc(Initial3, -1, Initial1),
		 arc(Initial3, -1, Initial2),
		 arc(Final1, -1, Final3),
		 arc(Final2, -1, Final3)
		 | Arcs_
		].

mult_nfa(nfa(States1, Arcs1, Initial1, [Final1]),
	 nfa(States2, Arcs2, Initial2, [Final2])
	) :-

	States2 = [Initial2, Final2 |States1],
	Arcs2 = [arc(Initial2, -1, Final2),
		 arc(Initial2, -1, Initial1),
		 arc(Final1, -1, Final2),
		 arc(Final1, -1, Initial1)
		 | Arcs1
		].







