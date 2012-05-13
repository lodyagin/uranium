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
%
% This object store HTTP/1.1 headers

% TODO X-Headers and other headers

% For reference, X-headers are also referred to as x-token
% in the BNF of RFC 2045, as user-defined ("X-") in
% section 5 of RFC 2047 and as Experimental headers in
% section 4.2.2.1 of the News Article Format draft.
% Deprecating Use of the "X-" Prefix in Application
% Protocols (draft 02; October 24, 2011):

:- module(http_headers_v,
          [http_headers_list_obj/2,   % ?List, ?Obj
           send_headers/2             % +Stream, +Obj
           ]).

/* TODO

   rfc2616:   General-header field names can be extended reliably only in
   combination with a change in the protocol version. However, new or
   experimental header fields may be given the semantics of general
   header fields if all parties in the communication recognize them to
   be general-header fields. Unrecognized header fields are treated as
   entity-header fields.
*/

:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(u(ur_atoms)).
:- use_module(u(v)).
:- use_module(u(internet/rfc822)).

new_class(http_headers_v, object_v, []).

new_class(http_general_headers_v, http_headers_v,
          [
           % rfc 2616, 4.5 General Header Fields
           cache_control,
           connection,
           date,
           pragma,
           trailer,
           transfer_encoding,
           upgrade,
           via,
           warning
          ]).

new_class(http_entity_headers_v, http_headers_v,
          [
           % rfc 2616, 7.1 Entity Header Fields
           allow,
           content_encoding,
           content_language,
           content_length,
           content_location,
           content_md5,
           content_range,
           content_type,
           expires,
           last_modified,
           extension_header
          ]).

new_class(http_request_headers_v, http_headers_v,
          [
           % rfc 2616, 5.3 Request Header Fields
           accept,
           accept_charset,
           accept_encoding,
           accept_language,
           authorization,
           expect,
           from,
           host,
           if_match,
           if_modified_since,
           if_none_match,
           if_range,
           if_unmodified_since,
           max_forwards,
           proxy_authorization,
           range,
           referer,
           te,
           user_agent
          ]).

new_class(http_request_with_origin_headers_v, http_headers_v,
          [
           % rfc 6454
           origin
          ]).

new_class(http_response_headers_v, http_headers_v,
          [
           % rfc 2616, 6.2 Response Header Fields
           accept_ranges,
           age,
           etag,
           location,
           proxy_authenticate,
           retry_after,
           server,
           vary,
           www_authenticate
          ]).

% rfc 6265
new_class(http_response_with_cookies_headers_v, http_headers_v,
          [
           % rfc 6265, 5.2.  The Set-Cookie Header
           set_cookie
          ]).

new_class(http_request_with_cookies_headers_v, http_headers_v,
          [
           % rfc 6265, 5.4.  The Cookie Header
           cookie
          ]).

new_class(http_experimental_1_0_request_headers_v, http_request_headers_v,
          [keep_alive]).

new_class(http_websocket_general_headers_v, http_headers_v,
          [
           % rfc 6455, 4.3
           sec_websocket_extensions,
           sec_websocket_protocol,
           sec_websocket_version
          ]).

new_class(http_websocket_request_headers_v, http_headers_v,
          [
           % rfc 6455, 4.3, client headers
           sec_websocket_key
          ]).

new_class(http_websocket_response_headers_v, http_headers_v,
          [
           % rfc 6455, 4.3, server headers
           sec_websocket_accept
          ]).
           
% It contains not empty @bulk or contains mixed
% request/response headers or miss required fields
new_class(http_invalid_headers_v, http_headers_v, []).

% TODO add http_invalid_miss_headers_v on missed required fields

new_class(http_invalid_mixed_headers_v,
          http_invalid_headers_v, []).

new_class(http_invalid_bulk_headers_v,
          http_invalid_headers_v,
          [
           '@bulk'  % unparsed (invalid) headers live here
                    % in the form of Header = Value list
                    % It is always ground if the object is
                    % constructed by http_headers_list_obj
           ]).

% http_headers_list_obj(?List, ?Obj)
%
% Conversion between headers in the form Header = Value
% list and http_headers_v obj family.  At least one
% argument must be instantiated

http_headers_list_obj(List, Obj) :-

   Ctx = context(http_headers_list_obj/2, _),
   http_headers_list_obj_cmn(List, Obj, Ctx).

% % http_headers_list_obj(?List0, ?List, ?Obj)
% %
% % This is a difference list version of http_headers_list_obj/2

% http_headers_list_obj(List0, List, Obj) :-

%    Ctx = context(http_headers_list_obj/3, _),
%    http_headers_list_obj_cmn(List0, List, Obj, Ctx).


% http_headers_list_obj_cmn(+List, -Obj, Ctx)

http_headers_list_obj_cmn(List, Obj, Ctx) :-

   nonvar(List), !,
   obj_construct(http_headers_v, [], [], Obj0),
   findall(fields(Class, Fields),
           (  member(Type,
                     [general, entity, request, response,
                      experimental_1_0_request,
                      websocket_general,
                      request_with_origin,
                      request_with_cookies,
                      response_with_cookies,
                      websocket_request,
                      websocket_response
                      ]
                    ),
              concat_atom([http, Type, headers_v], '_',
                          Class),
              class_fields(Class, Fields)
           ),
           Class_Fields),
   http_headers_list_obj(List, Class_Fields,
                         Obj0, Obj1, [], Bulk, Ctx),
   (  Bulk == [],
      \+ obj_same_or_descendant(Obj1,
                     http_invalid_bulk_headers_v), !
   ;
      Bulk \= [],
      obj_field(Obj1, '@bulk', Bulk)
   ),
   mix_case(Obj1, Obj2),

   Normal_Parents_Order = [http_invalid_bulk_headers_v,
                           http_invalid_mixed_headers_v,
                           http_invalid_headers_v,
                           http_entity_headers_v,
                           http_websocket_response_headers_v,
                           http_response_with_cookies_headers_v,
                           http_response_headers_v,
                           http_websocket_request_headers_v,
                           http_request_with_cookies_headers_v,
                           http_experimental_1_0_request_headers_v,
                           http_request_with_origin_headers_v,
                           http_request_headers_v,
                           http_websocket_general_headers_v,
                           http_general_headers_v,
                           http_headers_v,
                           object_v, object_base_v],

   obj_sort_parents(Obj2, Normal_Parents_Order, Obj).

% http_headers_list_obj(-List0, +Obj, +Ctx)

http_headers_list_obj_cmn(List, Obj, _) :-

   nonvar(Obj), !,
   obj_rewrite(Obj, weak, ['@bulk'], [Bulk], [_], Obj1),
   ignore(Bulk = []),
   obj_list(Obj1, List1),
   append(List1, Bulk, List2),
   maplist(header_prolog_http, List2, List).

http_headers_list_obj_cmn(_, _, Ctx) :-

   % At least one arg should be instantiated
   throw(error(instantiation_error, Ctx)).


http_headers_list_obj([], _,
                      Obj, Obj, Bulk, Bulk, _) :- !.

http_headers_list_obj([Option|Tail], Class_Fields,
                      Obj0, Obj, Bulk0, Bulk, Ctx) :-

   nonvar(Option),
   (  Option = (Header0 = Value1)
   ;  functor(Option, Header0, 1), arg(1, Option, Value1)
   ),
   !,

   header_prolog_http(Header=Value, Header0=Value1),

   must_be(atom, Header),
   (  obj_field(Obj0, fail, Header, Value)
   -> Obj1 = Obj0, Bulk1 = Bulk0
   ;  downcast_headers(Header, Value, Class_Fields,
                       Obj0, Obj1, Bulk0, Bulk1, Ctx)
   ),
   http_headers_list_obj(Tail, Class_Fields,
                         Obj1, Obj, Bulk1, Bulk, Ctx).

http_headers_list_obj(X, _, _, _, _, _, Ctx) :-

   throw(error(type_error(option_list, X), Ctx)).


downcast_headers(Header, Value, Class_Fields,
                 Obj0, Obj, Bulk0, Bulk, _) :-

   (  member(fields(Class, Fields), Class_Fields),
      ord_memberchk(Header, Fields)
   ->
      obj_rebase((http_headers_v -> Class), Obj0, Obj),
      obj_field(Obj, Header, Value),
      Bulk = Bulk0
   ;
      (  obj_same_or_descendant(Obj0,
                     http_invalid_bulk_headers_v)
      -> Obj = Obj0
      ;  obj_rebase((http_headers_v ->
                     http_invalid_bulk_headers_v),
                    Obj0, Obj)
      ),
      Bulk = [Header = Value|Bulk0]
   ).

% rebase to http_invalid_mixed_headers_v if there is a mix
mix_case(Obj0, Obj) :-

   obj_parents(Obj0, Parents0),
   list_to_ord_set(Parents0, Parents1),
   (  ord_memberchk(http_request_headers_v, Parents1),
      ord_memberchk(http_response_headers_v, Parents1)
   ->
      obj_rebase((http_headers_v ->
                  http_invalid_mixed_headers_v),
                 Obj0, Obj)
   ;
      Obj = Obj0
   ).

% Translate between prolog and http header formats (names)

header_prolog_http(PHeader=Value_Obj, HHeader=Value) :-

   nonvar(PHeader), !,
   obj_field(Value_Obj, body, Value),
   (  concat_atom(PTokens, '_', PHeader)
   -> maplist(capitalize_atom, PTokens, HTokens),
      concat_atom(HTokens, '-', HHeader)
   ;  capitalize_atom(PHeader, HHeader)
   ).

header_prolog_http(PHeader=Value_Obj, HHeader=Value) :-

   nonvar(HHeader), !,
   (  concat_atom(HTokens, '-', HHeader)
   -> maplist(downcase_atom, HTokens, PTokens),
      concat_atom(PTokens, '_', PHeader)
   ;  downcase_atom(HHeader, PHeader)
   ),
   obj_construct(http_header_v, [name, body], [PHeader, Value],
                 Value_Obj0),
   obj_downcast(Value_Obj0, Value_Obj).

send_headers(Stream, Obj) :-

   http_headers_list_obj(List, Obj),
   out_headers_list(List, Stream).

out_headers_list([], _) :- !.

out_headers_list([Header=Value|T], Stream) :-

   format(Stream, '~a: ~a\r\n', [Header, Value]),
   out_headers_list(T, Stream).



