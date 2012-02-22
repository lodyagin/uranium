:- module(regex, []).

expr --> expr2 | [].

expr2 --> part, "|", expr2 | part.

part --> left, part | left.

left -->  mult | solid .

mult --> solid, "*".

solid --> "(", expr2, ")" | letter.

letter -->
	[C], { is_usual_symbol(C) }.

is_usual_symbol(C) :-

   \+ member(C, "()*|").

