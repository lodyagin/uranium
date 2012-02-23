:- begin_tests(regex).
:- use_module(u(regex/regex)).

test(syntax_positive,
     [forall(
	  member(Pat,
		 ["", "a", "ab", "a*", "a*a", "a|b", "a|b|c",
		  "(a)", "((a))", "(((ab)))",
		  "b*|b", "ab*b",
		  "(a|b)", "(a*|b)", "(ab*|b)", "(b*|b*)",
		  "(b*|b*)*", "(b|a|c)*", "b|a|c*", "a|ab*|abc*",
		  "(a|ab*|abc*)d", "(a|ab*|abc*)(a)d",
		  "(a|ab*|abc*)(a*)d", "(a|ab*|abc*)(a*)(d)",
		  "(a|ab*|abc*)(a*)(d)|e*", "(a|ab*|abc*)(a*)(d)|e",
		  "(a|ab*|abc*)(a*)(d)|(e|d*)",
		  "((a|ab*|abc*)(a*)(d)|(e|d*))",
		  "(((a|ab*|abc*)(a*)(d)|(e|d*)))"
		 ])
	    )
     ]) :-

	regex_nfa(Pat, _).

	%findall('.', phrase(regex(_), Pat, []), ['.']).


test(syntax_negative,
     [forall(
	  member(Pat,
		 ["*", "**", "|*", "()", "(*)", "()*",
		  "*a", "*|b", "*|*", "|a", "|a|b", "|a|",
		  "b|", "a|b|", "ab*|", "ab|", "|ab", "|ab|",
		  "(a|b", "a|b)", "(a*|b", "a|b*)", "(b*|b*)**",
		  "*(a|v)", "(a|ab*|abc*)()d",
		  "(a|ab*|abc*)(a*)(d)|*"
		  ])
	  ), fail
     ]) :-

	regex_nfa(Pat, _).

test(nfa_n_states,
     [forall(member((Pat, N_States), [("ab*", 5)]))]
    ) :-

	regex_nfa(Pat, nfa(States, _, _, _)),
	numlist(1, N_States, States).



:- end_tests(regex).

