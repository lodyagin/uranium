%  -*- fill-column: 65; -*-
% 
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012, Kogorta OOO Ltd.
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

%  This contains predicates for interation with a user.

:- module(ur_term_dialog,
          [prompt/4  % +Msg, +Args, +Keys_LOL, -Result
           ]).

:- use_module(library(error)).
:- use_module(u(internal/decode_arg)).

% prompt(+Msg, +Args, +Keys_LOL, -Result)

prompt(Msg, Args, Keys_LOL, Result) :-

   Ctx = context(prompt/4, _),
   must_be(atom, Msg),
   must_be(list, Args),
   must_be(list(list(atom)), Keys_LOL),

   format(Msg, Args),
   prompt2(Keys_LOL, Result, Ctx),
   format('~a~n', [Result]).

prompt2(Keys_LOL, Result, Ctx) :-
  
   get_single_char(Code), char_code(Char, Code),

   catch(
         decode_arg(Keys_LOL, Char, Result, Ctx),
         Error,
         (   Error = error(domain_error(Possible_Vals, _), _)
         ->  format('~nPlease enter one of ~p ', [Possible_Vals]),
             prompt2(Keys_LOL, Result, Ctx)
         ;
             throw(Error)
         )
        ).

