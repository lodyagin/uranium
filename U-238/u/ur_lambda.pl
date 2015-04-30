% It has another concept than u(util/lambda).
% It doesn't copy terms so constraints (attributes) are preserved.

:- module(ur_lambda,
          [free_vars/2,      % @Templ, -Var1
           free_vars/3,      % @Templ, -Var1, -Var2
           free_vars/4       % @Templ, -Var1, -Var2, -Var3
          ]).

:- meta_predicate free_vars(0, ?).
:- meta_predicate free_vars(0, ?, ?).
:- meta_predicate free_vars(0, ?, ?, ?).

%% free_vars(@Templ, -Var1) is det.
free_vars(Templ0, Var1) :-
   copy_term(Templ0, Templ),
   term_variables(Templ, [Var1|_]),
   call(Templ).

%% free_vars(@Templ, -Var1, -Var2) is det.
free_vars(Templ0, Var1, Var2) :-
   copy_term(Templ0, Templ),
   term_variables(Templ, [Var1, Var2|_]),
   call(Templ).

%% free_vars(@Templ, -Var1, -Var2, -Var3) is det.
free_vars(Templ0, Var1, Var2, Var3) :-
   copy_term(Templ0, Templ),
   term_variables(Templ, [Var1, Var2, Var3|_]),
   call(Templ).

