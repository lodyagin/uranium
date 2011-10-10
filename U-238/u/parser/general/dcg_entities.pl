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

:- module(dcg_entities,
          [ipv4//1,
           port//1,
           ipv4_and_port//2,
           digits_any//1
           ]).

:- use_module(library(http/dcg_basics)).

%
% Take as few as possible digits from the input, taking one more
% each time on backtracking.
% This code is normally followed by a test for a delimiter.
%

digits_any([]) -->
           [].
digits_any([H|T]) -->
           digit(H),
           digits_any(T).
          

ipv4(IP) -->

        digits_any(C1), ".", !, { ip_comp(C1, A) },
        digits_any(C2), ".", !, { ip_comp(C2, B) },
        digits_any(C3), ".", !, { ip_comp(C3, C) },
        digits(C4),
        { ip_comp(C4, D),
          atomic_list_concat([A, B, C, D], '.', IP)
        }.

ip_comp(Codes, Num) :-

        % not empty sequence
        Codes \= [],
        % not starting from 0 if more than 1 digit
        Codes = [H|T], (T = [] -> true ; \+ atom_codes('0', [H])),
        % =< 255
        number_codes(Num, Codes),
        Num =< 255.

port(Port) -->

        digit(H), digits(T),
        { number_codes(Port, [H|T]),
          Port =< 65535
        }.

ipv4_and_port(IP, Port) -->

        ipv4(IP), ":", port(Port).