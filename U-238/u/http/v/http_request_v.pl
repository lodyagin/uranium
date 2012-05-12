% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose
% functional test platform.
%
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it
% and/or modify it under the terms of the GNU Lesser
% General Public License as published by the Free Software
% Foundation; either version 2.1 of the License, or (at
% your option) any later version.
%
% This library is distributed in the hope that it will be
% useful, but WITHOUT ANY WARRANTY; without even the
% implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser
% General Public License along with this library; if not,
% write to the Free Software Foundation, Inc., 51 Franklin
% Street, Fifth Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(http_request_v, []).

/** <module> HTTP request by rfc2616.

  ---+++ Parent
  v(http_message_v)

  ---+++ New static fields
   * method

   * request_uri
*/

new_class(http_request_v, http_message_v,
          [method,
           request_uri
          ]).

new_class(http_options_request_v, http_request_v, []).

new_class(http_get_request_v, http_request_v, []).

new_class(http_head_request_v, http_request_v, []).

new_class(http_post_request_v, http_request_v, []).

new_class(http_put_request_v, http_request_v, []).

new_class(http_delete_request_v, http_request_v, []).

new_class(http_trace_request_v, http_request_v, []).

new_class(http_connect_request_v, http_request_v, []).

'http_request_v?'(Obj, class, Class) :-

   obj_field(Obj, method, Method),
   (  var(Method) -> true
   ;  concat_atom([http, Method, request_v], '_', Class)
   ).

