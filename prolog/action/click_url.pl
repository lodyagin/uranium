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

:- module(click_url, [click_url/2,
                      click_url/3,
                      click_url/4,
                      click_with_all_redirects/4
                      %+User, +URL_Expr, ?Page, [+]Proxy
                      ]).

:- use_module(html/http_ops).
:- use_module(lib/sl_objects).
:- use_module(html/http_page). 

% TODO add actor (cookies etc.)

click_url(URL_Expr, Page) :- click_url(URL_Expr, Page, _).


click_url(URL_Expr, Page, Proxy) :-

  click_url_with_cookies(URL_Expr, Page, Proxy, _).


click_url(User, URL_Expr, Page, Proxy) :-

  obj_field(User, cookie_db_key, Cookies_DB),
  click_url_with_cookies(URL_Expr, Page, Proxy, Cookies_DB).


click_url_with_cookies(URL_Expr, Page, Proxy, Cookies_DB) :-

  Options1 = [],
  eval_obj_expr(URL_Expr, URL),
  (  nonvar(Proxy)
  -> obj_field(Proxy, ip, Proxy_IP),
     obj_field(Proxy, port, Proxy_Port),
     selectchk(proxy(Proxy_IP, Proxy_Port), Options, Options1)
  ;  Options = Options1
  ),
  http_page(URL, http_ops:http_get_html(Options, Cookies_DB), Page).


% Perform all redirects

click_with_all_redirects(User, URL_Expr, Page, Proxy) :-

  click_url(User, URL_Expr, Page1, Proxy),
  perform_redirects(User, Page1, Proxy, Page).

perform_redirects(_, Page, _, Page) :-

  \+ functor(Page, redirect_page_v, _), !.

perform_redirects(User, Page1, Proxy, Page2) :-

  click_with_all_redirects(User, Page1 / url_to, Page2, Proxy).


