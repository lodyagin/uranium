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

new_class(link_v, http_result_v, [uri, text], [uri]).

new_class(local_link_v, link_v, []).

new_class(global_link_v, link_v, []).

'link_v?'(Link, class, Class) :-

   obj_field(Link, uri, Uri),
   uri_data(authority, Uri, Domain),
   (  var(Domain)
   -> Class = local_link_v
   ;  Class = global_link_v
   ).

/*
downcast(link_v, local_link_v, Link, Local_Link) :-

   functor(Link, link_v, _),
   functor(Local_Link, local_link_v, _),

   obj_field(Link, url, Url),
   uri_components(Url, Base_Uri),
   obj_field(Local_Link, base_uri, Base_Uri).
*/