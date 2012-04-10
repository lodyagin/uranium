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


:- module(http_ops,
          [http_do/6,
           url_host_path/3
          ]).

:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(u(html/cookies_man)).
:- use_module(u(logging)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/decode_arg)).

% for use DOM parsing
:- use_module(library(http/http_sgml_plugin)).

:- multifile http_client:open_connection/4.

% http_do(+Method, +Options, @Cookies_DB, +URL, @Post_Data, -DOM)
%
% Get/Post data and return a page as DOM
%

http_do(Method, Options, Cookies_DB, URL, Post_Data, DOM) :-

   Ctx = context(http_do/6, _),
   check_inst(URL, Ctx),
   decode_arg([[post], [get]], Method, Method1, Ctx),
   must_be(list, Options),
   (  var(Cookies_DB) -> true
   ;  check_db_key(Cookies_DB, Ctx)
   ),
   (  Method = get -> true
   ;  check_inst(Post_Data, Ctx)
   ),

   get_cookies_headers(Cookies_DB, URL, Headers),

   append(Options,
          [reply_header(Reply_Headers) | Headers],
          Options2),

    write_log(['http_do(', Method, ', ..., ', URL, '), options:',
               Options2],
              [logger(http_ops), lf(1, before), lf(1)]),

    (  Method1 = get
    -> http_get(URL, Data, Options2)
    ;  http_post(URL, Post_Data, Data, Options2)
    ),

    write_log(Data,
              [logger(http_ops), lf(1, before), lf(1)]),
    write_log(['reply headers:', Reply_Headers],
              [logger(http_ops), lf(1, before), lf(1)]),

    (   nonvar(Cookies_DB)
    ->  url_host_path(URL, Host, Path),
        store_cookies(Cookies_DB, Reply_Headers, Host, Path)
    ;   true
    ),

    (   Data = redirect(Redirect),
        nonvar(Redirect)
    ->  write_log(['redirect to', Redirect],
                  [logger(http_ops), lf(1, before), lf(1)]),
        parse_url(Redirect, URL, New_URL),
        http_do(get, Options, Cookies_DB, New_URL, _, DOM)
    ;   DOM = Data
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




