% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2012, Kogorta OOO Ltd
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

:- module(www_address_v, []).

/** <module> Abstraction of an address in WWW
*/

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internet/rfc6454)). % origin
:- use_module(u(http/cookies_man)).

new_class(www_address_v, object_v,
          [http_request_url,  % original URL of the request
           http_response_url, % != http_request_url if
                              % it is a redirected page
                              % Must be absolute and normalized.
           cookies_db,
           cookies_id,

           http_request_headers,
           http_response_headers,  % Usualy contains timestamp
           http_proxy
           ],

          [http_request_url,
           http_response_url,
           http_request_headers,
           http_response_headers,
           http_proxy
           ]
         ).

'www_address_v?'(Obj, request_origin, Origin) :-

   obj_field(Obj, http_request_url, Uri),
   uri_origin(Uri, Origin).

'www_address_v?'(Obj, response_origin, Origin) :-

   obj_field(Obj, http_response_url, Uri),
   uri_origin(Uri, Origin).

copy(www_address_v, From, To) :-

   % TODO changke cookies_id instead of cookies_db
   obj_rewrite(From, [cookies_db], [Old_DB_Key], [New_DB_Key], To),

   (   ground(Old_DB_Key)
   ->  new_cookie_db_key(New_DB_Key),
       db_copy(Old_DB_Key, New_DB_Key)
   ;   true
   ).

% TODO class invalid_www_address as desc of www_address (must
% contain required fields)


