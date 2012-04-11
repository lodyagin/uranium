% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012, Kogorta OOO Ltd
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
%
%  Basic http operations


:- module(http_ops,
          [http_do/7, % +Method, +Options, ?Headers, @Cookies_DB, +URL,
                      % @Post_Data, -DOM

           http_do/8, % +Method, +Options, ?Headers, @Cookies_DB, +URL,
                      % @Post_Data, -DOM, -Redirect_Steps

           url_host_path/3  % +URL, -Host, -Path
          ]).

:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(u(http/cookies_man)).
:- use_module(u(http/v/http_headers_v)).
:- use_module(u(logging)).
:- use_module(u(v)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_url)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).

% for use DOM parsing
:- use_module(library(http/http_sgml_plugin)).

:- multifile http_client:open_connection/4.

% http_do(+Method, +Options, ?Headers, @Cookies_DB, +URL,
%         @Post_Data, -DOM)
%
% Get/Post data and return a page as DOM
% If Headers is unbound unify it with request headers.
%

http_do(Method, Options, Headers, Cookies_DB, URL, Post_Data, DOM) :-

   Ctx = context(http_do/7, _),
   http_do_cmn(Method, Options, Headers, Cookies_DB, URL,
               Post_Data, DOM, _, Ctx).

% http_do(+Method, +Options, ?Headers, @Cookies_DB, +URL,
%         @Post_Data, -DOM, -Redirect_Steps)
%
% Redirect_Steps list contains all pages visited
%

http_do(Method, Options, Headers, Cookies_DB, URL, Post_Data, DOM,
        Redirect_Steps) :-

   Ctx = context(http_do/8, _),
   http_do_cmn(Method, Options, Headers, Cookies_DB, URL,
               Post_Data, DOM, Redirect_Steps, Ctx).

http_do_cmn(Method, Options, Headers, Cookies_DB, URL, Post_Data,
            DOM, Redirect_Steps, Ctx) :-

   must_be(atom, URL),
   decode_arg([[post], [get]], Method, Method1, Ctx),
   must_be(list, Options),
   (  var(Headers)
   -> obj_construct(http_headers_v, [], [], Headers)
   ;  check_object_arg(Headers, Ctx, _)
   ),
   (  var(Cookies_DB) -> true
   ;  check_db_key(Cookies_DB, Ctx)
   ),
   (  Method1 = get -> true
   ;  check_inst(Post_Data, Ctx)
   ),

   http_do_int(Method1, Options, Headers, Cookies_DB, URL,
               Post_Data, DOM, [], Redirect_Steps_Rev, Ctx),
   reverse(Redirect_Steps_Rev, Redirect_Steps).

http_do_int(Method, Options, Headers, Cookies_DB, URL, Post_Data,
            DOM, Redirect_Steps0, Redirect_Steps, Ctx) :-

   http_headers_list_obj(Request_Headers, Headers),
   get_cookies_headers(Cookies_DB, URL, Cookies_Headers),

   append(Options,
          [reply_header(Reply_Headers0) | Cookies_Headers],
          Options1),
   findall(request_header(H),
           member(H, Request_Headers),
           RH_Options),
   append(Options1, RH_Options, Options2),

   write_log(['http_do(', Method, ', ..., ', URL, '), options:',
              Options2],
             [logger(http_ops), lf(1, before), lf(1)]),

   (  Method = get
   -> http_get(URL, Data, Options2)
   ;  http_post(URL, Post_Data, Data, Options2)
   ),

   % The option which is not headers/cookies, see
   % http://www.swi-prolog.org/pldoc/man?predicate=http_read_request/2
   remove_options(Reply_Headers0,
                  [input, method, path, peer, port, request_uri,
                   search, http_version, cookie, status(_, _)],
                  Reply_Headers1),

   (   nonvar(Cookies_DB)
   ->  url_host_path(URL, Host, Path),
       store_cookies(Cookies_DB, Host, Path, Reply_Headers1, Reply_Headers)
   ;   remove_options(Reply_Headers1, [set_cookie], Reply_Headers)
   ),

   http_headers_list_obj(Reply_Headers, Reply_Headers_Obj),

   obj_construct(www_address_v,
                 [http_request_url, http_response_url,
                  http_request_headers, http_response_headers,
                  cookies_db, cookies_id],
                 [URL, URL, Headers, Reply_Headers_Obj,
                  Cookies_DB, _],
                 WWW_Address),

    write_log(Data,
              [logger(http_ops), lf(1, before), lf(1)]),
    write_log(['reply headers:', Reply_Headers],
              [logger(http_ops), lf(1, before), lf(1)]),

    Redirect_Steps1 = [WWW_Address|Redirect_Steps0],
    (   Data = redirect(Redirect),
        nonvar(Redirect)
    ->  write_log(['redirect to', Redirect],
                  [logger(http_ops), lf(1, before), lf(1)]),
        url_normalize(Redirect, URL, New_URL),
        http_do_int(get, Options, Headers, Cookies_DB, New_URL, _,
                    DOM, Redirect_Steps1, Redirect_Steps, Ctx)
    ;   DOM = Data, Redirect_Steps = Redirect_Steps1
    ).

get_cookies_headers(Cookies_DB_Key, URL, Headers) :-

         nonvar(Cookies_DB_Key) ->

         % prepare cookies
         url_host_path(URL, Host, Path),
         retrieve_cookies_headers(Cookies_DB_Key, Host, Path,
                                  Headers)
        ;
         Headers = [].


% extract host and path from URL (for cookie retrieving)
%
% url_host_path(+URL, ?Host, ?Path)
%

url_host_path(URL, Host, Path) :-

   atom(URL), !,
   uri_normalized(URL, URL_Norm),
   uri_components(URL_Norm, Components),
   uri_data(path, Components, Path2),
   uri_data(authority, Components, Auth),
   uri_authority_components(Auth, AC),
   uri_authority_data(host, AC, Host),

   (Path2 = '' -> Path = '/' ; Path = Path2).

url_host_path(URL, Host, Path) :-

   is_list(URL), !,
   memberchk(host(Host), URL),
   memberchk(path(Path), URL).
   % NB path component must be

url_host_path(URL, _, _) :-

   throw(error(type_error(atom_or_list, URL),
               context(url_host_path/3, _))).

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
        format(user_error, 'Accepting certificate~n', []).

http_client:open_connection(https, Host:Port, In, Out) :-

        http_open:open_socket(Host:Port, PlainIn, PlainOut, []),

	ssl_context(client, SSL, [ host(Host),
				   port(Port),
                                   cert_verify_hook(cert_verify),
				   close_parent(true)
				 ]),
        catch(ssl_negotiate(SSL, PlainIn, PlainOut, In, Out),
              Exception,
              ( ssl_exit(SSL), throw(Exception)) ).




