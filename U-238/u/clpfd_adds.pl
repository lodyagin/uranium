%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(clpfd_adds,
          [
           drep_nth0/3
          ]).

:- use_module(library(clpfd)).

% drep_nth0(+N, +Drep, -X)
% Return the nth value in the interval (drep).
% Drep should be normalized (e.g. result of fd_dom)

drep_nth0(N, Drep, K) :-

  drep_nth0(Drep, N, _, K),
  nonvar(K).

drep_nth0(Drep1 \/ Drep2, N0, N, K) :-

  drep_nth0(Drep1, N0, N1, K),
  (  var(K)
  -> drep_nth0(Drep2, N1, N, K)
  ;  true ), !.

drep_nth0(I..J, N0, N, K) :-

  (  I + N0 =< J
  -> K is I + N0
  ;  N is N0 - (J - I + 1)
  ), !.
  
drep_nth0(I, 0, _, I) :- !.

drep_nth0(_, N0, N, _) :- 

  N is N0 - 1.
  

