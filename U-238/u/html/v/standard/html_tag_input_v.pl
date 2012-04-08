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

%  All types of html input tags

:- module(html_tag_input_v, []).

:- use_module(u(v)).
:- use_module(html_tag_v).

new_class(html_tag_input_v, html_tag_v,
          [
           '.disabled',
           '.name',
           '.onblur',
           '.onfocus',
           '.onselect',
           '.size',
           '.type',
           '.value'
           ]).

new_class(html_tag_input_button_v, html_tag_input_v, []).

new_class(html_tag_input_checkbox_v, html_tag_input_v,
          ['.checked']).

new_class(html_tag_input_file_v, html_tag_input_v,
          ['.accept']).

new_class(html_tag_input_hidden_v, html_tag_input_v, []).

new_class(html_tag_input_image_v, html_tag_input_v,
          ['.align',
           '.alt',
           '.src'
           ]).

new_class(html_tag_input_password_v, html_tag_input_v,
          ['.maxlength',
           '.readonly'
           ]).

new_class(html_tag_input_radio_v, html_tag_input_v,
          ['.checked']).

new_class(html_tag_input_reset_v, html_tag_input_v, []).

new_class(html_tag_input_submit_v, html_tag_input_v, []).

new_class(html_tag_input_text_v, html_tag_input_v,
          ['.maxlength',
           '.readonly'
           ]).

'html_tag_input_v?'(Obj, class, Class) :-

   obj_field(Obj, '.type', Type0),
   (  atom(Type0),
      ( Type0 = txt -> Type = text ; Type = Type0 )
   -> concat_atom(['html_tag_input_', Type, '_v'], '', Class),
      class_exists(Class)
   ;  functor(Obj, Class, _)
   ).

downcast(html_tag_input_v, html_tag_input_button_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_checkbox_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_file_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_hidden_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_image_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_password_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_radio_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_reset_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_submit_v, From, To) :-
   gen_html_tag_downcast(From, To).


downcast(html_tag_input_v, html_tag_input_text_v, From, To) :-
   gen_html_tag_downcast(From, To).


   

