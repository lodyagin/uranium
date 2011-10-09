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

:- module(http_proxy_v,
          []).

:- use_module(parser/general/dcg_entities).
:- use_module(library(http/dcg_basics)).
:- use_module(library(ur_lists)).

new_class(thread_object_v, object_v, [thread_id]).

new_class(http_proxy_v, thread_object_v,
          [
           source,
           dnl_time   : timestamp,
           check_time : timestamp,
           ip         : ip_addr,
           port       : integer,
           status,
           log        : binary,
           response_time : time_interval
          ],
          [ip, port]).

typedef(timestamp,
        [pretty_print - timestamp_pretty_print,
         postgres     - type('timestamp with time zone',
                             timestamp_pl_pg)
        ]).

typedef(ip_addr, [postgres - type(inet, =)]).

typedef(time_interval,
        [postgres - type(interval, time_interval_pl_pg)]).

typedef(binary, [postgres - type(bytea, binary_pl_pg)]).

% PL_BIN can be atom, list of codes or term
binary_pl_pg(PL_BIN, PG_BIN) :-

   nonvar(PL_BIN), !,
   (  PL_BIN = codes(Codes_Unlim)
   -> true
   ;  atom(PL_BIN)
   -> atom_codes(PL_BIN, Codes_Unlim)
   ;  compound(PL_BIN)
   -> term_to_atom(PL_BIN, Atom_Unlim),
      atom_codes(Atom_Unlim, Codes_Unlim)
   ),

   list_head(Codes_Unlim, 512, Codes, _),
   codes_bytea(Codes, Bytea_Codes),
   atom_codes(PG_BIN, Bytea_Codes).

binary_pl_pg(PL_BIN, PG_BIN) :-

   nonvar(PG_BIN), !,
   (  atom(PG_BIN)
   -> PG_Atom = PG_BIN
   ;  term_to_atom(PG_BIN, PG_Atom)
   ),
   atom_codes(PG_Atom, Bytea_Codes),
   codes_bytea(Codes, Bytea_Codes),
   atom_codes(PL_BIN, Codes).

codes_bytea([], []) :- !.

codes_bytea([92|T], [92, 92, 92, 92|ET]) :- codes_bytea(T, ET), !.

% will be doubled later by ' PG EscQuoteToDbQ1'
codes_bytea([39|T], [39|ET]) :- codes_bytea(T, ET), !.

codes_bytea([NP|T], Bytea) :- 

   nonvar(NP),
   (between(0, 31, NP); between(127, 255, NP)), !,
   number_octet(NP, Octet),
   append([92, 92], Octet, Escaped_NP),
   codes_bytea(T, ET),
   append(Escaped_NP, ET, Bytea), !.

codes_bytea([NP|T], [92|OT]) :-

   var(NP),
   phrase(octet(NP), OT, ET),
   codes_bytea(T, ET), !. 
   
codes_bytea([H|T], [H|ET]) :- codes_bytea(T, ET), !.

number_octet(Number, Octet) :-

   format(codes(S), '~8r', Number), 
   length(S, N), 
   Low is N - 3, 
   High is N - 1, 
   findall(X, 
           (between(Low, High, Y), (Y < 0 -> X = 48 ; nth0(Y, S, X))), 
           Octet).


octet(Number) -->

   digit(D0),
   digits(D),
   { L = [D0|D],
     length(L, N),
     Low is N - 3,
     High is N - 1,
     findall(X, 
             (between(Low, High, Y), (Y < 0 -> X = 48 ; nth0(Y, L, X))), 
             [O1, O2, O3]),
     Number is ((O1 - 48) << 3 + (O2 - 48)) << 3 + (O3 - 48)
   }.
     

timestamp_pretty_print(Stream, Value, _) :-

   format_time(Stream, '%d %b %T', Value).

timestamp_pl_pg(PL_TS, PG_TS) :-

   ground(PL_TS), !,
   format_time(atom(PG_TS), '%F %T%z', PL_TS).

timestamp_pl_pg(PL_TS, PG_TS) :-

   ground(PG_TS), !,
   atom_codes(PG_TS, Codes),
   phrase(pg_timestamp(PL_TS), Codes).


time_interval_pl_pg(PL_I, PG_I) :-

   ground(PL_I), !,
   format(atom(PG_I), '~f', [PL_I]).

time_interval_pl_pg(PL_I, PG_I) :-

   term_to_atom(PG_I, PG_IA),
   atom_codes(PG_IA, Codes),
   phrase(pg_interval(PL_I), Codes).

pg_interval(Interval) -->

   digits_any(H), ":", { number_codes(Hours, H) },
   digits_any(M), ":", { number_codes(Mins, M) },
   number(Secs),

   { Interval is 60 * ((60 * Hours) + Mins) + Secs }.

pg_timestamp(TS) -->

   digits_any(Y), "-",  { number_codes(Year, Y) },
   digits_any(M), "-",  { number_codes(Month, M) },
   digits_any(D), " ",  { number_codes(Day, D) },
   digits_any(H), ":",  { number_codes(Hour, H) },
   digits_any(Mi), ":", { number_codes(Min, Mi) },
   number(Sec),
   integer(Off_Hours),

   { Off_Secs is Off_Hours * 3600,
   
     date_time_stamp(date(Year, Month, Day, Hour, Min, Sec,
                          Off_Secs, -, -),
                     TS)
   }, !.

