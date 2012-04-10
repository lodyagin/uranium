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
%  This module emulates browser clicks.

:- module(click_url, [click_url/2,  % +URL_Expr, -Page
                      click_url/3,  % +URL_Expr, @Proxy, -Page
                      click_url/5,  % +URL_Expr, @Proxy, +User0,
                                    % -User, -Page
                      
                      click_with_all_redirects/2,
                      %+URL_Expr, -Page nondet
                      click_with_all_redirects/4,
                      %+URL_Expr, +User0, -User, -Page nondet
                      click_with_all_redirects/5
                      %+URL_Expr, @Proxy, +User0, -User, -Page nondet
                      ]).

:- use_module(u(html/http_ops)).
:- use_module(u(v)).
:- use_module(u(html/http_page)). 

% click_url(+URL_Expr, -Page)

click_url(URL_Expr, Page) :-

   click_url(URL_Expr, _, Page).


% click_url(+URL_Expr, @Proxy, -Page)

click_url(URL_Expr, Proxy, Page) :-

  click_url_with_cookies(URL_Expr, Proxy, _, Page).


% click_url(+URL_Expr, @Proxy, +User0, -User, -Page)

click_url(URL_Expr, Proxy, User0, User, Page) :-

  obj_field(User0, cookie_db_key, Cookies_DB),
  click_url_with_cookies(URL_Expr, Proxy, Cookies_DB, Page),
  obj_field(Page, http_request_url, Page_Url),
  obj_rewrite(User0, [current_url], _, [Page_Url], User).


click_url_with_cookies(URL_Expr, Proxy, Cookies_DB, Page) :-

  Options1 = [],
  eval_obj_expr(URL_Expr, URL),
  (  nonvar(Proxy)
  -> obj_field(Proxy, ip, Proxy_IP),
     obj_field(Proxy, port, Proxy_Port),
     selectchk(proxy(Proxy_IP, Proxy_Port), Options, Options1)
  ;  Options = Options1
  ),
  http_page(http_ops:http_get_html(Options, Cookies_DB), URL,
            Page).


% click_with_all_redirects(+URL_Expr, @Proxy, +User0, -User, -Page)
% is nondet
%
% Perform all redirects. Try all Page interpretations

click_with_all_redirects(URL_Expr, Proxy, User0, User, Page) :-

  click_url(URL_Expr, Proxy, User0, User1, Page1),
  obj_reinterpret(Page1, Page2),
  % <NB> redirect does not change current_url in User
  perform_redirects(Proxy, User1, Page2, User, Page).

click_with_all_redirects(URL_Expr, User0, User, Page) :-

   click_with_all_redirects(URL_Expr, _, User0, User, Page).

click_with_all_redirects(URL_Expr, Page) :-

  click_url(URL_Expr, Page1),
  obj_reinterpret(Page1, Page2),
  perform_redirects(Page2, Page).


% A redirect of a not redirect page is the same page
perform_redirects(_, User, Page, User, Page) :-

   \+ obj_same_or_descendant(Page, redirect_page_v), !.

perform_redirects(User, Page1, Proxy, Page2) :-

  click_with_all_redirects(User, Page1 / url_to, Proxy, Page2).


% A redirect of a not redirect page is the same page
perform_redirects(Page, Page) :-

   \+ obj_same_or_descendant(Page, redirect_page_v), !.

perform_redirects(Page1, Page2) :-

  click_with_all_redirects(Page1 / url_to, Page2).

