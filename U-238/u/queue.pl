% -*- fill-column: 58; -*-
% _________________________________________________________
%
% This file is a part of Uranium, a general-purpose prolog
% library.
%
% This library is free software; you can redistribute it
% and/or modify it under the terms of the GNU Lesser
% General Public License as published by the Free Software
% Foundation; either version 2.1 of the License, or (at
% your option) any later version.
%
% This library is distributed in the hope that it will be
% useful, but WITHOUT ANY WARRANTY; without even the
% implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser
% General Public License along with this library; if not,
% write to the Free Software Foundation, Inc., 51 Franklin
% Street, Fifth Floor, Boston, MA 02110-1301 USA
% _________________________________________________________
%
% The original implementation of queue taken from the book
% "The Craft of Prolog" by Richard O'Keefe.
% _________________________________________________________

:- module(queue,
          [list_queue/2,
           queue/1,
           queue/2,
           queue_head/3,
           queue_head_list/3,
           queue_last/3,
           queue_last_list/3,
           queue_length/2
           ]).

% queue(?Queue) is semidet.
% is true when Queue is a queue with no elements.

queue(q(0, B, B)).

% queue(?El, ?Queue) is semidet.
% is true when Queue is a queue with one element

queue(El, q(s(0), [El|Y], Y)).

% queue_head(?X, ?Q1, ?Q0) is semidet.
% is true when Q0 and Q1 have the same elements except
% that Q0 has in addition X at the front.
% Use it for enqueuing and dequeuing both.

queue_head(X, q(N, F, B), q(s(N), [X|F], B)).

% queue_head_list(?List, ?Q1, ?Q0) is semidet.  is true
% when append(List, Q1, Q0) whould be true if only Q1 and
% Q0 were lists instead of queues.

queue_head_list([], Q, Q).
queue_head_list([X|Xs], Q1, Q) :-
  queue_head(X, Q1, Q2),
  queue_head_list(Xs, Q2, Q).

% queue_last(?X, ?Q1, ?Q0) is semidet.
% is true when Q0 and Q1 have the same elements except
% that Q0 has in addition X at the end.

queue_last(X, q(N, F, [X|B]), q(s(N), F, B)).

% queue_last_list(?List, ?Q1, ?Q0) is semidet.
% is true when append(Q1, List, Q0) whould be true if only
% Q1 and Q0 were lists instead of queues.

queue_last_list([], Q, Q).
queue_last_list([X|Xs], Q1, Q) :-
  queue_last(X, Q1, Q2),
  queue_last_list(Xs, Q2, Q).

% list_queue(?List, Queue) is semidet.
% is true when List is a list and Queue is a queue and
% they represent the same sequence.

list_queue(List, q(N, F, B)) :-
  list_queue(List, N, F, B).

list_queue([], 0, B, B).
list_queue([X|Xs], s(N), [X|F], B) :-
  list_queue(Xs, N, F, B).

% queue_length(+Queue, -Length)

queue_length(q(N, F, B), L) :-
  queue_length(N, F, B, 0, L).

queue_length(0, B, B, L, L).
queue_length(s(N), [_|F], B, L0, L) :-
  succ(L0, L1),
  queue_length(N, F, B, L1, L).







