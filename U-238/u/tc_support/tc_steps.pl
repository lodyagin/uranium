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

%  -*-coding: mule-utf-8-unix; fill-column: 58-*-
%
%  Copyright (C) 2009, 2011 Kogorta
%
%  Description      : Реализация шагов тест кейса.
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Apr 7 2009
%

:- module(tc_steps,
          [step/3,
           step/4,
           main_batch/4,
           main_batch/5,
           sub_batch/3%,
%           step_map/3
          ]).

:- use_module(u(action/check_result)).
:- use_module(u(logging)).
:- use_module(u(v)).
:- use_module(u(tc_support/tc_log)).

% Выполнение тест-кейса
%
% main_batch(+Name, :Initialize, Step_List, :Finalize)
%
main_batch(Name, Initialize, Step_List, Finalize) :-
    main_batch(Name, Initialize, Step_List, Finalize, []).

main_batch(Name, Initialize, Step_List, Finalize, Options) :-
    check_option_list(Options, [pause(_), gtrace(_)]),
    write_log([Name, start]),
    \+ memberchk(step_num(_), Options),
    catch(
          ignore(
                 call_cleanup((Initialize, !,
                               run_series(Step_List,
                                          [step_num([]) | Options])
                              ),
                              Catcher,
                              (Finalize, !,
                               Catcher = exit, Passed=true)
                             )
                ),
          Exception,
          write_log(Exception)
         ),
    (ground(Passed), Passed=true
    ->
     write_log([Name, 'PASSED'], [lf(2, before), lf(2, after)])
    ;
     write_log([Name, 'FAILED'], [lf(2, before), lf(2, after)]),
     fail
    ).

run_series(Series, Options) :-
    maplist(call_with_options(Options), Series).

call_with_options(Options, Goal) :-
    % Накладываем опции только если это step
    functor(Goal, F, _),
    atom_concat(step, _, F) % начинается со step
    ->
    call(Goal, Options)
    ;
    (memberchk(gtrace(Step_Num), Options),
     memberchk(step_num([Step_Num|_]), Options)
     -> gtrace; true),
    Goal.

% Выполнение части тест-кейса
%
% sub_batch(+Name, +Step_List)
%
sub_batch(Name, Step_List, Options) :-
    write_log(['Sub batch', Name, 'starts'], [lf(2,before), lf(1, after)]),
    catch(
          ignore((run_series(Step_List, Options), Passed = true)),
          Exception,
          write_log(Exception)
         ),
    (ground(Passed), Passed=true
    ->
     true
    ;
     fail
    ).

%
% Выводит номер шага в форме num1 [ / num2 [/ num3 ...] ]
% на основе обратного списка [..., num3, num2, num1].
%
format_step_number([Step|Oversteps]) :-
    reverse(Oversteps, List),
    maplist(format("~w / "), List),
    format("~w", Step).

step(Num, Exec_List, Check_List) :-
    step(Num, Exec_List, Check_List, []).

step(Num, Exec_List, Check_List, Options) :-

    number(Num),

    % Реализация паузы.
    (   memberchk(pause(before), Options)
    ->  get_char(_);
        memberchk(pause(Pause_Value), Options),
        number(Pause_Value)
    ->  make_pause(Pause_Value);
        true
    ),

    % Отслеживание вложенности шагов.
    selectchk(step_num(Step_Num_List), Options, Options2),
    New_Step_Num_List = [Num | Step_Num_List],
    New_Options = [step_num(New_Step_Num_List) | Options2],

    % Вывод номера шага.
    with_output_to(string(Step_Num_S),
                   format_step_number(New_Step_Num_List)
                  ),
    format(string(S1), "[.] Step ~s", Step_Num_S),
    write_log(S1, [module(no), lf(2, before), lf(2, after)]),

    % Выполнение шага.
    (
       % The part `execute'
       (is_list(Exec_List)
       ->
        maplist(action_do(New_Options), Exec_List, Exec_Session_List),
        sort(Exec_Session_List, Session_List2) % delete repeats
       ;
        action_do(New_Options, Exec_List, Exec_Session),
        Session_List2 = [Exec_Session]
       ),
       % The part `check'
       (is_list(Check_List)
       ->
        maplist(action_do(New_Options), Check_List, _)
       ;
        action_do(New_Options, Check_List, _)
       )

    ->
     % Вывод 'passed'
     write_log([step, S1, passed])

     % Очистка всех сообщений
     %write_log(['Clear messages on', Session_List2]),
     %maplist(clear_state_rev(user_message), Session_List2)
    ;
     % Вывод 'failed' и дампа состояния.
     %write_log('', [module(no), lf(2)]),
     %state:dump_session(_, []),
     %write_log('', [module(no)]),

     write_log([step, S1, failed], [lf(2)]),
     fail % fail the test case
    ),

    % Реализация паузы.
    (memberchk(pause(after), Options) -> get_char(_); true).

% Переворачивает аргументы state:clear_state/2
clear_state_rev(Object, Session) :- clear_state(Session, Object).

%
% action_do(+Options, :Goal, -Exec_Session)
%
% Выполнение Goal с передачей Options.
% Если Goal = execute* и получается определить сессию
% из первого параметра, то Exec_Session становится определено.
%
action_do(Options, Goal, Exec_Session) :-
     var(Exec_Session),
     (
     Goal \= true % экономим на действиях с целью true (=nop)
     ->

     % Определяем сессию (на основе первого аргумента
     % всех целей execute*, по соглашению).
     (functor(Goal, F, _), atom_concat(execute, _, F)
     ->
      (arg(1, Goal, Exec_First_Term),
       eval_obj_expr(Exec_First_Term, Exec_First_Object),
       obj_field(Exec_First_Object, session, Exec_Session), !
      ;
       true
      )
     ;
      true),  % в случае, когда не можем определить сессию,
              % Exec_Session не определена

     % Логируем запуск цели.
     write_log('before: ', [module(no), lf(no), quoted(no)]),
     log_step_goal(Goal, [module(no), lf(no), quoted(no)]),

     % Запуск
     catch(call_with_options(Options, Goal),
           true,
           true
          ), !,

     write_log('after: ', [module(no), lf(no), quoted(no)]),
     log_step_goal(Goal, [module(no), lf(no), quoted(no)]),

     % Отчет о выполнении
     write_log(' |passed|', [module(no), lf(2)])

     ;

     true  % in case of execute or check part = 'true' - no call, no log
    ).


make_pause(Value) :-

   write_log([pause, Value, 'sec(s)'],
             [module(no), lf(1, before), lf(1)]),
   sleep(Value).


step_map(Action, Params_List, Options) :-

   Action =.. [Action_Fun|Action_Pars],
   member(Params, Params_List),
   append(Action_Pars, Params, Goal_Pars),
   Goal =.. [Action_Fun|Goal_Pars],
   (   action_do(Options, Goal, _)
   ->  fail
   ;   !, fail
   )
   ;
   true.


