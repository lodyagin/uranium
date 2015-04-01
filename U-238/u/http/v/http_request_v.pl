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

:- use_module(library(random)).
:- use_module(u(gt/gt_strings)).
:- use_module(library(clpfd)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

new_class(http_request_v, http_message_v,
          [method : http_method,
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

typedef(http_method, [value_set - http_method_set_gen]).

:- meta_predicate http_method_set_gen(:, -).

http_method_set_gen(Options, Value) :-

   options_to_object(http_method_set_gen, Options, Opt),

   obj_unify(Opt,
             [http_method_type, length, generator, seed],
             [Method_Types, Length, Generator, Seed]),

   random_member(Method_Type, Method_Types),

   (  Method_Type = http_method_standard
   -> random_member(Value, [options, get, head, post, put, delete, trace,
                            connect]) %TODO use Gen
   ;
      /* rfc2616:
       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT
        */
      C in 0..127,
      #\ C in 0..31 \/ 127, % CTL
      #\ C in 0'( \/ 0') \/ 0'< \/ 0'> \/ 0'@ \/ 0', \/ 0'; \/ 0': \/ 0'\\
              \/ 0'" \/ 0'/ \/ 0'[ \/ 0'] \/ 0'? \/ 0'= \/ 0'{ \/ 0'} %'"
              \/ 32 \/ 9,
      fd_dom(C, Token_Chars),
      random_string([Length, Generator, Seed, range(Token_Chars)],
                    atom(Value))
   ).

