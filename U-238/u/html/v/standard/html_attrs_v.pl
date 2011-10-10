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

% It is attributes for (mostly) all html tags

:- module(html_attrs_v,
          [
           construct_html_attrs/3,
           unify_html_attrs/4
           ]).

new_class(html_attrs_v, object_v,
          ['.id',
           '.class',
           '.title',
           '.style',
           '.dir',
           '.xml:lang',
           '.@bulk']).

unify_html_attrs(_, [], Rest, Rest) :- !. 
unify_html_attrs(Obj, [Id = Val|T], Rest0, Rest) :-
   atom_concat('.', Id, Field),
   (  obj_field_wf(Obj, Field, Val)
   -> Rest1 = Rest0
   ;  Rest1 = [Id = Val|Rest0] ),
   unify_html_attrs(Obj, T, Rest1, Rest).

construct_html_attrs(Class, Attr_List, Obj) :-

   obj_construct(Class, [], [], Obj),
   unify_html_attrs(Obj, Attr_List, [], Rest),
   obj_field(Obj, '.@bulk', Rest).

downcast(html_attrs_v, html_input_attrs_v, From, To) :-

   obj_field(From, '.@bulk', Old_Bulk),
   construct_html_attrs(html_input_attrs_v, Old_Bulk, To).
   % other attributes are copied by Uranium
   