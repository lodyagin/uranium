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
                   ixpath/4,
                   
                   op(400, fx, //),
                   op(400, fx, /),
                   op(200, fy, @)
                   ]).

:- use_module(library(lists)).
:- use_module(library(ur_objects)).


% ixpath(+Spec, +Dom, -Result)

ixpath(Spec, Dom, Result) :-

   Ctx = context(ixpath/3, _),
   ixpath2(Spec, Dom, [], Result, Ctx).


% ixpath(+Spec, +Dom, +Options, -Result)

ixpath(Spec, Dom, Options, Result) :-

   Ctx = context(ixpath/4, _),
   ixpath2(Spec, Dom, Options, Result, Ctx).


ixpath2(Spec, Dom, Options, Result, Ctx) :-

   (  (var(Spec); var(Dom); var(Options))
   -> throw(error(instantiation_error, Ctx))
   ;  (Options = [] | Options = [_|_])
   -> true
   ;  throw(error(type_error(list, Options), Ctx))
   ),
   (  Dom = [Real_Dom]
   -> xpath(Spec, Real_Dom, Options, Result)
   ;  functor(Dom, element, 3)
   -> (  ground(Dom)
      -> xpath(Spec, Dom, Options, Result)
      ;  throw(error(instantiation_error, Ctx))
      )
   ;  u_object(Dom)
   -> obj_field(Dom, dom, Dom2),
      ixpath(Spec, Dom2, Options, Result)
   ;  throw(error(type_error(dom, Dom), Ctx))
   ).


xpath(Spec, Dom, Options, Result) :-

   xpath(Spec, Dom, Options, [], Path_R, Result),
   (  memberchk(tag_path_rev(Tag_Path_R), Options)
   -> dom_tag_path(Path_R, Tag_Path_R)
   ;  memberchk(tag_path(Tag_Path), Options)
   -> reverse(Path_R, Path),
      dom_tag_path(Path, Tag_Path)
   ;  memberchk(tag_attr_path(Attr_List, Tag_Attr_Path), Options)
   -> reverse(Path_R, Path),
      dom_tag_attr_path(Path, Attr_List, Tag_Attr_Path)
   ;  true
   ).


% DOM elements path -> tags path
dom_tag_path([], []) :- !.
dom_tag_path([element(Tag, _, _)|DT], [Tag|TT]) :-
   dom_tag_path(DT, TT).

% DOM elements path -> tags with selected attrs path
dom_tag_attr_path([], _, []) :- !.
dom_tag_attr_path([element(Tag, Attrs, _)|DT],
                  Attrs_Query,
                  [Tag_With_Attrs|TT]) :-
   
   extract_attrs(Attrs_Query, Attrs, Attr_Vals),
   Tag_With_Attrs =.. [Tag|Attr_Vals],
   dom_tag_attr_path(DT, Attrs_Query, TT).

% TODO O(N*M)
extract_attrs([], _, []) :- !.
extract_attrs([Attr|AQT], Attrs, [Val|AVT]) :-
   ignore(memberchk(Attr=Val, Attrs)),
   extract_attrs(AQT, Attrs, AVT).

% xpath essentials

xpath(Head/Tag, Dom, Options, Path0, Path, Result) :-

   xpath(Head, Dom, Options, Path0, Path1, Result1),
   xpath(/Tag, Result1, Options, Path1, Path, Result).

xpath(Head//Tag, Dom, Options, Path0, Path, Result) :-

   xpath(Head, Dom, Options, Path0, Path1, Result1),
   xpath(//Tag, Result1, Options, Path1, Path, Result).

xpath(/Tag, Dom, Options, Path0, Path, Result) :-

   Dom = element(_, _, Sub_Elements),
   member(Sub_Element, Sub_Elements),
   xpath(Tag, Sub_Element, Options, Path0, Path, Result).
   
xpath(//Tag, Dom, Options, Path0, Path, Result) :-

   (  xpath(/Tag, Dom, Options, Path0, Path, Result)
   ;  xpath(/(*)//Tag, Dom, Options, Path0, Path, Result)
   ).
   
xpath(Tag, Dom, _, Path0, [Dom|Path0], Result) :-

   atom(Tag),
   Dom = element(Real_Tag, _, _), % also skip html text elements
   (Tag == '*' -> true; Real_Tag = Tag),
   Result = Dom.

xpath(Expr, Dom, Options, Path0, Path, Result) :-

   compound(Expr),
   Expr =.. [Tag|Cond_List],
   xpath(Tag, Dom, Options, Path0, Path, Result),
   check_cond_list(Cond_List, Result).


% check conditions

check_cond_list([], _) :- !.
check_cond_list([Cond|TC], Result) :-
   check_cond(Cond, Result),
   check_cond_list(TC, Result).


check_cond(@Attr=Value, element(_, Attributes, _)) :-

   atom(Attr),
   memberchk(Attr=Value, Attributes).
