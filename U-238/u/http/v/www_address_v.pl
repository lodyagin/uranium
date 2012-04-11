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
%
% This module defines an abstraction of an address in WWW.

:- module(www_address_v, []).

new_class(www_address_v, object_v,
          [http_request_url,  % original URL of the request
           http_response_url, % != http_request_url if
                              % it is a redirected page
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

% TODO invalid_www_address -> www_address (must contain required fields)


