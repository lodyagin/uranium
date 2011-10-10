:- module(ypath,
          [
           dom_walker/2,
           get_level/3,
           print_el/2,
           print_level/2,
           print_tree/1
          ]).

:- use_module(u(ixpath)).

print_tree(DOM) :-

   ixpath((*)//(*), DOM, [tag_attr_path([class, id], Path)], _),
   print_path(Path), nl,
   fail ; true.

print_path([]) :- !.
print_path([El

get_level(Level, DOM, Tag_Path) :-

   integer(Level), Level >= 0,
   make_xpath_expr(Level, Expr),
   !,
   ixpath(Expr, DOM, [tag_path(Tag_Path)], _).

print_level(Level, DOM) :-

   integer(Level), Level >= 0,
   make_xpath_expr(Level, Expr),
   !,
   ixpath(Expr, DOM, [tag_path(Tag_Path)], _),
   writeln(Tag_Path),
   fail ; true.

make_xpath_expr(0, '*') :- !.
make_xpath_expr(N, /(Expr, '*')) :-
   N1 is N - 1,
   make_xpath_expr(N1, Expr).

dom_walker(Filter, DOM) :-

   dom_walker2(Filter, DOM, []).

dom_walker2(_, [], _) :- !.

dom_walker2(Filter, [E|T], L) :- !,

   dom_walker2(Filter, E, L),
   dom_walker2(Filter, T, L).

dom_walker2(Filter, E, L) :-

   E = element(_, _, M), !,
   reverse(L, LR),
   call(Filter, E, LR),
   dom_walker2(Filter, M, [E|L]).

dom_walker2(_, _, _).

print_el(E, []) :-

   E = element(Tag, _, _),
   xpath(E, /'*'(normalize_space), T),
   format("~a: ~a~n", [Tag, T]).

print_el(E, [element(F, _, _)|L]) :-

   format("~a > ", F),
   print_el(E, L).
