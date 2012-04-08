%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(http_ops,
          [http_get_html/4,
           http_post_html/5,
           url_host_path/3
          ]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(u(html/cookies_man)).
:- use_module(u(logging)).

% for use DOM parsing
:- use_module(library(http/http_sgml_plugin)). 

:- multifile http_client:open_connection/4.

% Get url as DOM
%
% http_get_html(+Options, ?Cookies_DB_Key, +URL, -DOM)
%
% If Cookies_DB_Key is not bound do not send cookies
%

http_get_html(Options, Cookies_DB, URL, DOM) :-

        get_cookies_headers(Cookies_DB, URL, Headers),

        append(Options,
               [reply_header(Reply_Headers) | Headers],
               Options2),

        write_log(['http_get URL:', URL, ', options:', Options2],
                  [logger(http_ops), lf(1, before), lf(1)]),

        http_get(URL, Data, Options2), !,
        
        write_log(Data,
                  [logger(http_ops), lf(1, before), lf(1)]),
        write_log(['reply headers:', Reply_Headers],
                  [logger(http_ops), lf(1, before), lf(1)]),

        (   nonvar(Cookies_DB)
        ->  url_host_path(URL, Host, Path),
            store_cookies(Cookies_DB, Reply_Headers,
                          Host, Path)
        ;   true
        ),

        (   Data = redirect(Redirect),
            nonvar(Redirect)
        ->  write_log(['redirect to', Redirect],
                      [logger(http_ops), lf(1, before), lf(1)]),
            parse_url(Redirect, URL, New_URL),
            http_get_html(Options, Cookies_DB, New_URL, DOM)
        ;   DOM = Data
        ).

%
% Post data and return a page as DOM
%

http_post_html(Options, Cookies_DB, Post_Data, URL, DOM) :-

   get_cookies_headers(Cookies_DB, URL, Headers),
   append(Options,
          [reply_header(Reply_Headers) | Headers],
          Options2),

    write_log(['http_post URL:', URL, ', options:', Options2],
              [logger(http_ops), lf(1, before), lf(1)]),

    http_post(URL, Post_Data, Data, Options2),
        
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
        http_get_html(Options, Cookies_DB, New_URL, DOM)
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




