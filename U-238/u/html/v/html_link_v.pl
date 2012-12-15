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

:- module(html_link_v, []).

:- use_module(library(uri)).
:- use_module(u(v)).

new_class(html_link_v, html_tag_a_v, [text], [www_address, '.href']).

% A link to the same domain
new_class(html_local_link_v, html_link_v, []).

% A link to another domain
new_class(html_global_link_v, html_link_v, []).

'html_link_v?'(Link, class, Class) :-

   obj_field(Link, '.href', Url),
   atom(Url),
   (  \+ uri_is_global(Url)
   ->
      Class = html_local_link_v
   ;
      eval_obj_expr(Link / www_address / http_request_url,
                    Base_Url),
      atom(Base_Url),
      uri_resolve(Url, Base_Url, Global_Url),
      uri_components(Base_Url, Base_Url_Comps),
      uri_components(Global_Url, Global_Url_Comps),
      uri_data(authority, Base_Url_Comps, Base_Domain),
      uri_data(authority, Global_Url_Comps, Link_Domain),

      (   Base_Domain == Link_Domain
      ->  Class = html_local_link_v
      ;   Class = html_global_link_v
      )
   ).

%downcast(

downcast(link_v, html_local_link_v, From, To) :-

   obj_field(From, '.href', Orig_Link_Url),
   atom(Orig_Link_Url),
   obj_field(From, www_address, WWW_Address),
   atom(WWW_Address),
   obj_field(WWW_Address, http_request_url, Base_Url0),
   atom(Base_Url0),
   uri_normalized(Base_Url0, Base_Url),
   uri_normalized(Orig_Link_Url, Base_Url, Link_Url),
   obj_field(To, '.href', Link_Url),
   obj_field(To, www_address, WWW_Address).

downcast(link_v, html_global_link_v, From, To) :-

   obj_field(From, '.href', Orig_Link_Url),
   atom(Orig_Link_Url),
   obj_field(From, www_address, WWW_Address),
   atom(WWW_Address),
   obj_field(WWW_Address, http_request_url, Base_Url0),
   atom(Base_Url0),
   uri_normalized(Base_Url0, _), %Base_Url),
   uri_normalized(Orig_Link_Url, Link_Url),
   obj_field(To, '.href', Link_Url),
   obj_field(To, www_address, WWW_Address).



