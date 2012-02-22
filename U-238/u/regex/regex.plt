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

	findall('.', phrase(regex:expr, Pat, []), ['.']).


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

	phrase(regex:expr, Pat, []).


:- end_tests(regex).
