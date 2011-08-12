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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Copyright (C) 2009, 2011 Kogorta
%
%  Description      : Autotest execution logging.
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Mar 30 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Invoke SWI-Prolog with -p log_path=<log file path>
% if you want log in file.

:- module(logging, 
          [logged/1,  %:Goal (fail on exception)
           logged/2,  %:Goal, -?Exception (not fail)
           write_log/1, write_log/2,
           write_log_map/3,
           open_log/1,   % +Options
           log_piece/2,  % +Message, +Options
           close_log/1,  % +Options
           check_logger/1,  %+Options  true if to log (logger is enabled)
           change_indent/3,
           exclude_lf/2
          
]).

:- use_module(lib/sl_lists).

:- module_transparent write_log/1, write_log/2, logged/1, logged/2.

:- dynamic logger/2.
% logger(Name, State).
% State ::= enabled | disabled

exclude_lf(Oin, Oout) :-

  findall(O, (member(O, Oin), O \= lf(_), O \= lf(_,_)), Oout).

change_indent(Oin, Oout, Step) :-

  select(indent(N), Oin, O),
  integer(N)

  ->

  M is N + Step,
  select(indent(M), Oout, O), !

  ;

  (  S is Step, S > 0
  -> select(indent(S), Oout, Oin), !
  ;  Oout = Oin
  ).

%
% If exception occurs in Goal then catch it, log it and fail.
%
logged(Goal) :-
    catch(
          Goal,
          Exception,
          (write_log(Exception), fail)
         ).

% not fail but unify Exception
logged(Goal, Exception) :-
    catch(
          Goal,
          Exception,
          write_log(Exception)
         ).

% For use from maplist
write_log_map(Options, Message1, Message2) :-

  append(Message1, [Message2], Message),
  write_log(Message, Options).

%
% Write to all log files.
% Работает для как для одиночного значения так и для
% списка.
%
write_log(Message) :- write_log(Message, []).

write_log(Message, Options) :-

    check_logger(Options) ->
  
    open_log(Options),
    log_piece(Message, []),  %FIXME options
    close_log(Options)

    ; true.


write_piece(Stream, Options, Piece) :-

    memberchk(quoted(no), Options)
    ->
    format(Stream, "~w ", [Piece])
    ;
    format(Stream, "~q ", [Piece]).


check_logger(Options) :-

    % Если задан логгер, то он должен быть включен.
    (   memberchk(logger(Logger_Name), Options)
    -> logging:logger(Logger_Name, enabled), !
    ;  true
    ).

check_log_options(Options_In, Options_Out) :-

    check_option_list(Options_In,
                      [module(yes), module(no),
                       lf(_), lf(_,before), lf(_,after),
                       logger(_),
                       quoted(if_needed), quoted(no),
                       indent(_)
                      ]),
    
    % Обработка lf/1, lf/2

    % lf/1 -> lf/2
    (  select(lf(NLF1), Options_In, O1)
    -> O2 = [lf(NLF1, after) | O1]
    ;  O2 = Options_In
    ),

    % lf(no, A) -> lf(0, A)
    (  select(lf(no, LFA1), O2, O22)
    -> O3 = [lf(0, LFA1)|O22]
    ;  O3 = O2
%       (  memberchk(lf(_,_), O2)
%       -> O3 = O2
%       ;  O3 = [lf(1, after)|O2]  % default lf
%       )
    ),                         

    Options_Out = O3.

    
open_log(Options_In) :-

    check_log_options(Options_In, Options),

    check_logger(Options) ->
  
    nb_getval(logging_counter, Logging_Counter),

    (
        log_file(Stream),
        (
           % lf(N, before) 
           (  memberchk(lf(NLFB, before), Options),
              integer(NLFB),
              NLFB >= 0
           -> (between(1, NLFB, _), nl(Stream), fail; true)
           ; true
           ),
           format(Stream, "[~d]: ", Logging_Counter)
        ),
        fail % Can output to several logs.
    ;
        true
    ),

    succ(Logging_Counter, Next_Counter),
    nb_setval(logging_counter, Next_Counter)

    ; true.


log_piece(Message, Options_In) :-

    check_log_options(Options_In, Options),
  
    check_logger(Options) ->
  
    % Обработка quoted/1
    (   memberchk(quoted(Quoted), Options)
    ->  Qopt = quoted(Quoted)
    ;   Qopt = quoted(no)
    ),

    (
        log_file(Stream),
        (
           % indent(N)
           (  memberchk(indent(Indent), Options),
              integer(Indent),
              Indent > 0
           -> length(Spaces, Indent), maplist(=(32), Spaces),
              format('~s', [Spaces])
           ;  true
           ),
         
           (  is_list(Message)
           -> maplist(logging:write_piece(Stream, [Qopt]), Message)
           ;  logging:write_piece(Stream, [Qopt], Message)
           )
        ),

        % lf(N, after) 
        (  memberchk(lf(NLFA, after), Options),
           integer(NLFA),
           NLFA >= 0
        -> (between(1, NLFA, _), nl(Stream), fail; true)
        ;  true
        ),

        fail
    ;  % Can output to several logs.
        true
    )

    ; true.
    

close_log(Options_In) :-

    check_log_options(Options_In, Options),

    check_logger(Options) ->
  
    context_module(Module),
    (
        log_file(Stream),

        (  \+ memberchk(module(no), Options)
        -> format(" :[~a]", Module)
        ;  true
        ),
    
        % lf(N, after)

        % if no defined add by default (back comp)
        (  memberchk(lf(_,_), Options)
        -> O2 = Options
        ;  O2 = [lf(1, after)|Options]  % default lf
        ),
     
        (  memberchk(lf(NLFA, after), O2),
           integer(NLFA),
           NLFA >= 0
        -> (between(1, NLFA, _), nl(Stream), fail; true)
        ;  true
        ),

        flush_output(Stream),
        fail
    ;  % Can output to several logs.
        true
    )

    ; true.
  

start_logging :-
        (
         file_search_path(log_path, Path) 
      ->
         % If -p log_path is not given use the current
         % output string.
         open(Path, append, Stream),
         
         get_time(TS), stamp_date_time(TS, Date_Time, local),
         format(Stream, "--- New log started at ~w~n", [Date_Time])
      ;
         current_output(Stream)
        ),

        abolish(log_file/1),
        assertz(log_file(Stream)),
        nb_setval(logging_counter, 1).
%        nb_setval(logging_next_counter, 1).

        
        
:- initialization start_logging.        
