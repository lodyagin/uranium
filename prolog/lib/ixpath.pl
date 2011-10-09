%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
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

:- module(ixpath, [ixpath/3,
                   op(400, fx, //),
                   op(400, fx, /),
                   op(200, fy, @)
                   ]).

% ixpath(+Spec, +Dom, -Result)

ixpath(Spec, Dom, Result) :-

   (  (var(Spec); \+ground(Dom))
   -> throw(error(instantiation_error, context(ixpath/3, _)))
   ;  true),
   (  Dom = [Real_Dom]
   -> xpath(Spec, Real_Dom, Result)
   ;  xpath(Spec, Dom, Result)
   ).

xpath(Head/Tag, Dom, Result) :-

   xpath(Head, Dom, Result1),
   xpath(/Tag, Result1, Result).

xpath(/Tag, Dom, Result) :-

   Dom = element(_, _, Sub_Elements),
   member(Sub_Element, Sub_Elements),
   xpath(Tag, Sub_Element, Result).
   
xpath(Tag, Dom, Result) :-

   atom(Tag),
   (  Tag == '*'
   -> functor(Dom, element, 3) % skip html text elements
   ;  Dom = element(Tag, _, _)
   ),
   Result = Dom.

xpath(Expr, Dom, Result) :-

   compound(Expr),
   Expr =.. [Tag|Cond_List],
   xpath(Tag, Dom, Result),
   check_cond_list(Cond_List, Result).


check_cond_list([], _) :- !.

check_cond_list([Cond|TC], Result) :-

   check_cond(Cond, Result),
   check_cond_list(TC, Result).


check_cond(@Attr=Value, element(_, Attributes, _)) :-

   memberchk(Attr=Value, Attributes).
