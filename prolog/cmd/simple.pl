:- module(simple, [go/0]).

:- use_module(library(readutil)).

:- use_module(action/click_url).
:- use_module(library(ur_objects)).

go :-

   repeat,
   read_line_to_codes(current_input, Codes),
   atom_chars(Durty_Line, Codes),
   normalize_space(atom(Line), Durty_Line),
   (  Line \== end -> exec(Line), fail ;  true ),
   !.

exec(Line) :-

   click_url(Line, Page),
   obj_pretty_print(Page).
