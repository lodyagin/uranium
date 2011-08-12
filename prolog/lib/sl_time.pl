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
%  Description      : Работа с датой и временем.
%
%  Author           : Sergei Lodyagin
%
%  Created On       : Apr 1 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(sl_time, [date_to_YYYYMMDDHH24MISS/2]).

% Parse time in ICTS format
date_to_YYYYMMDDHH24MISS(Date, Time_String) :-
    Date = date(YYYY, Mon, Day, HH24, Min, Sec, 0 /*Unknown*/, -, -),
    (
    var(Time_String)
       ->
    format_time(atom(Time_String), '%Y%m%d%H%M%S', Date)
       ;
    atom_length(Time_String, 14),
    sub_atom(Time_String, 0, 4, _, YYYY_A), atom_number(YYYY_A, YYYY),
    sub_atom(Time_String, 4, 2, _, MM_A),   atom_number(MM_A, Mon),
    sub_atom(Time_String, 6, 2, _, DD_A),   atom_number(DD_A, Day),
    sub_atom(Time_String, 8, 2, _, HH24_A), atom_number(HH24_A, HH24),
    sub_atom(Time_String, 10, 2, _, MI_A),  atom_number(MI_A, Min),
    sub_atom(Time_String, 12, 2, _, SS_A),  atom_number(SS_A, Sec)
    ).
    
time_to_Atom24(time(H,M,S), T):-
	format_time(T,'%T',date(0,0,0,H,M,S,0,-,-)).
	
