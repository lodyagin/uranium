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

:- module(link_v, []).

:- use_module(library(uri)).
:- use_module(u(v)).

new_class(link_v, http_result_v, [link_url, text], [link_url]).

new_class(local_link_v, link_v, [], []).

new_class(global_link_v, link_v, [], []).

'link_v?'(Link, class, Class) :-

   obj_field(Link, link_url, Url),
   atom(Url),
   (  \+ uri_is_global(Url)
   ->
      Class = local_link_v
   ;
      obj_field(Link, http_request_url, Base_Url),
      atom(Base_Url),
      uri_resolve(Url, Base_Url, Global_Url),
      uri_components(Base_Url, Base_Url_Comps),
      uri_components(Global_Url, Global_Url_Comps),
      uri_data(authority, Base_Url_Comps, Base_Domain),
      uri_data(authority, Global_Url_Comps, Link_Domain),
      
      (   Base_Domain == Link_Domain
      ->  Class = local_link_v
      ;   Class = global_link_v
      )
   ).

downcast(link_v, local_link_v, From, To) :-

   obj_field(From, link_url, Orig_Link_Url),
   atom(Orig_Link_Url),
   obj_field(From, http_request_url, Base_Url0),
   atom(Base_Url0),
   uri_normalized(Base_Url0, Base_Url),
   uri_normalized(Orig_Link_Url, Base_Url, Link_Url),
   obj_field(To, link_url, Link_Url),
   obj_field(To, http_request_url, Base_Url).

downcast(link_v, global_link_v, From, To) :-

   obj_field(From, link_url, Orig_Link_Url),
   atom(Orig_Link_Url),
   obj_field(From, http_request_url, Base_Url0),
   atom(Base_Url0),
   uri_normalized(Base_Url0, Base_Url),
   uri_normalized(Orig_Link_Url, Link_Url),
   obj_field(To, link_url, Link_Url),
   obj_field(To, http_request_url, Base_Url).

   
   