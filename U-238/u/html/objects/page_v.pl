% -*- fill-column: 65; -*- 
% _____________________________________________________________
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
% 
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% _____________________________________________________________

:- module(page_v, ['page_v?'/3]).

:- use_module(library(ur_atoms)).
:- use_module(library(ur_objects)).
:- use_module(library(xpath)).
:- use_module(library(ixpath)).
:- use_module(library(js)).

new_class(html_piece_v, http_result_v, [dom]).

new_class(page_v, html_piece_v, [title]).

new_class(redirect_page_v, page_v, [url_to]).

'html_piece_v?'(Term, class, Value) :-

   obj_field(Term, dom, DOM),
   ground(DOM), !,
   (  ixpath(html, DOM, _)
   -> Value = page_v
   ;  Value = html_piece_v
   ).

%downcast(html_piece_v, page_v, Piece, Page) :-

%   obj_field(Piece, dom, DOM),
%   ground(DOM), !,
   
  
   
   

'page_v?'(Term, class, Value) :-

  obj_field(Term, dom, DOM),
  ground(DOM), !,
  (
   ( xpath(DOM, //html/body(@onload), On_Load)
   ; xpath(DOM, //script(normalize_space), On_Load),
     atom(On_Load)
   ),
   ( atom_concat('document.location.href=', _, On_Load)
   ; atom_concat('location.replace(', _, On_Load)
   )

  -> Value = redirect_page_v

  ; Value = page_v
  ).

downcast(page_v, redirect_page_v, Page, Redirect_Page) :-

  obj_field(Page, dom, DOM),
  ground(DOM), !,

  ( xpath(DOM, //html/body(@onload), On_Load)
  ; xpath(DOM, //script(normalize_space), On_Load),
    atom(On_Load)
  ),
  ( atom_concat('document.location.href=', Url_Str1, On_Load)
  ; atom_concat('location.replace(', Part2, On_Load),
    atom_concat(Url_Str1, ')', Part2)
  ), !,
  (  atom_concat(Url_Str2, ';', Url_Str1)
  -> true
  ;  Url_Str2 = Url_Str1
  ),
  remove_quotes(Url_Str2, Url_To3),
  js_decode_string(Url_To3, Url_To),
  obj_field(Redirect_Page, url_to, Url_To).
