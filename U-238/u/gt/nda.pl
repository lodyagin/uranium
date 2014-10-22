% (a|b)*ab*a

% e - 0
% a - 1
% b - 2

arcs(Arcs) :-
  Arcs =
  [ arc(0, 0, 1), arc(0, 0, 7),
    arc(1, 0, 2), arc(1, 0, 4),
    arc(2, 1, 3),
    arc(3, 0, 6),
    arc(4, 2, 5),
    arc(5, 0, 6),
    arc(6, 0, 1), arc(6, 0, 7),
    arc(7, 1, 8),
    arc(8, 0, 9), arc(8, 0, 11),
    arc(9, 2, 10),
    arc(10, 0, 8), arc(10, 0, 11),
    arc(11, 1, 12) ].

%automaton(Signature, [source(0), sink(12)], Arcs).

sign_var([], []) :- !.

sign_var([S|SL], VL) :- S == 0, !, sign_var(SL, VL).

sign_var([S|SL], [V|VL]) :-

  integer(S),
  between(1, 2, S),
  nth1(S, [a, b], V),
  sign_var(SL, VL).
