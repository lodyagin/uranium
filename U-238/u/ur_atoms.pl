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

:- module(ur_atoms,
          [tolower/2,
           trim_string/4,
           remove_quotes/2,
           replace_char/4,
           replace_chars/4
]).

:- use_module(ur_lists).

tolower(From, To) :-
    (
        atom(From), downcase_atom(From, To), !
    ;
        var(From), atom(To), downcase_atom(To, To), From = To
    ).

trim_string(Side, Untrimmed, Trimmed, Pred) :-
    string_to_list(Untrimmed, UL1),
    trim_list(Side, UL1, TL1, Pred),
    string_to_list(Trimmed, TL1).

remove_quotes(Q, NQ) :-

  (Quote = '\''; Quote = '"'),
  atom_concat(Quote, X, Q), !,
  atom_concat(NQ, Quote, X).

% replace_char(+Atom0, +Pos, +New, -Atom)
%
% Replace char to New at Pos in Atom0

replace_char(Atom0, Pos, New, Atom) :-

   Pos1 is Pos + 1,
   sub_atom(Atom0, 0, Pos, _, Prefix),
   sub_atom(Atom0, Pos1, _, 0, Suffix),
   concat_atom([Prefix, New, Suffix], '', Atom).

%
% replace_chars(+Atom1, ?Atom2, +Char1, +Char2)

replace_chars(Atom1, Atom2, Char1, Char2) :-

  atom(Atom1),
  atom_codes(Atom1, Codes1),
  atom_codes(Char1, [C1]),
  atom_codes(Char2, [C2]),
  maplist(replace_character_code(C1, C2), Codes1, Codes2),
  atom_codes(Atom2, Codes2).

replace_character_code(Code1, Code2, Code1, Code2) :- !.

replace_character_code(C1, _, Code1, Code1) :-

  C1 \= Code1.

 