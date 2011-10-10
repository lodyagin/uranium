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
                      click_with_all_redirects/2,
                      %+URL_Expr, -Page nondet
                      click_with_all_redirects/3,
                      %+User, +URL_Expr, -Page nondet
                      click_with_all_redirects/4
                      %+User, +URL_Expr, @Proxy, -Page nondet
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


% click_url(+User, +URL_Expr, @Proxy, -Page)

click_url(User, URL_Expr, Proxy, Page) :-

  obj_field(User, cookie_db_key, Cookies_DB),
  click_url_with_cookies(URL_Expr, Proxy, Cookies_DB, Page).


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


% click_with_all_redirects(+User, +URL_Expr, @Proxy, -Page)
% is nondet
%
% Perform all redirects. Try all Page interpretations

click_with_all_redirects(User, URL_Expr, Proxy, Page) :-

  click_url(User, URL_Expr, Proxy, Page1),
  obj_reinterpret(Page1, Page2),
  perform_redirects(User, Page2, Proxy, Page).

click_with_all_redirects(User, URL_Expr, Page) :-

   click_with_all_redirects(User, URL_Expr, _, Page).

click_with_all_redirects(URL_Expr, Page) :-

  click_url(URL_Expr, Page1),
  obj_reinterpret(Page1, Page2),
  perform_redirects(Page2, Page).


perform_redirects(_, Page, _, Page) :-

   functor(Page, Class, _),
   \+ (  Class == redirect_page_v
      ;  class_descendant(Class, redirect_page_v)
      ), !.

perform_redirects(User, Page1, Proxy, Page2) :-

  click_with_all_redirects(User, Page1 / url_to, Proxy, Page2).


perform_redirects(Page, Page) :-

   functor(Page, Class, _),
   \+ (  Class == redirect_page_v
      ;  class_descendant(redirect_page_v, Class)
      ), !.

perform_redirects(Page1, Page2) :-

  click_with_all_redirects(Page1 / url_to, Page2).

