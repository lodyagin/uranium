%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

% Pretty log for tc

:- module(tc_log, [log_step_goal/2]).

:- use_module(library(sl_objects)).
:- use_module(logging/logging).


log_step_goal(Goal, Options) :-

  open_log(Options),
  log_step_goal2(Options, Goal),
  close_log(Options).


log_step_goal2(Options_In, Goal) :-

  change_indent(Options_In, Options, 2),

  (  var(Goal)
  -> log_piece('_', Options)
  ;  atomic(Goal)
  -> log_piece(Goal, Options)
  ;  is_list(Goal)
  -> log_piece('[', Options),
     maplist(log_step_goal2(Options), Goal),
     log_piece(']', Options)
  ;  Goal =.. [Functor|Pars],
     (  atom_concat(_, '_v', Functor)
     -> obj_pretty_print(Options, Goal)
     ;  log2(Functor, Pars, Options)
     )
  ).
            

log2(Functor, [], Options_In) :-

  exclude_lf(Options_In, O),
  selectchk(lf(1), Options, O),
  log_piece(Functor, Options), !.


log2(Functor, Pars, Options_In) :-

  exclude_lf(Options_In, O),
  selectchk(lf(1), Options, O),
  atom_concat(Functor, '(', Head),
  log_piece(Head, Options),
  maplist(log_step_goal2(Options), Pars),
  log_piece(')', Options).

