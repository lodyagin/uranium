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
:- module(browser_v, []).

/** <module>  Class to hold all browser-dependent properties

  <NB> It contains only static part of the interaction
  For a dynamic part (like cookies etc.) see ../../http/v/http_user_v.pl
*/

new_class(browser_v, object_v,
          [browser, version, proxy_settings],
          [browser, version]).

new_class(firefox_browser_v, browser_v, []).

new_class(chrome_browser_v, browser_v, []).

new_class(firefox_browser_swipl_v, firefox_browser_v, []).

'browser_v?'(Obj, class, Class) :-

   obj_field(Obj, browser, Browser),
   (  Browser == firefox
   -> Class = firefox_browser_swipl_v
   ;  Browser == chrome
   ->  Class = chrome_browser_swipl_v
   ;  Class = browser_v
   ).

'browser_v?'(_, headers, Headers) :-

   obj_construct(http_request_headers_v, [], [], Headers).

% TODO ignore version, always 3.6.26
'firefox_browser_v?'(_, headers, Headers) :-

   obj_construct(http_experimental_1_0_request_headers_v,
                 [user_agent,
                  accept,
                  accept_language,
                  accept_encoding,
                  accept_charset,
                  keep_alive
                 ],
                 ['Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.26) Gecko/20120202 Iceweasel/3.6.26 (like Firefox/3.6.26)',
		  'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                  'en-us,en;q=0.5',
                  'gzip,deflate',
                  'ISO-8859-1,utf-8;q=0.7,*;q=0.7',
                  115
                  ], Headers).
'firefox_browser_swipl_v?'(_, headers, Headers) :-

   obj_construct(http_experimental_1_0_request_headers_v,
                 [user_agent,
                  accept,
                  accept_language,
                  accept_encoding,
                  accept_charset,
                  keep_alive
                 ],
                 ['Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.26) Gecko/20120202 Iceweasel/3.6.26 (like Firefox/3.6.26)'
		 ,
		  'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                  'en-us,en;q=0.5',
                  'deflate',
                  'ISO-8859-1,utf-8;q=0.7,*;q=0.7',
                  115
                  ], Headers).

'chrome_browser_v?'(_, headers, Headers) :-

   obj_construct(http_experimental_1_0_request_headers_v,
                 [connection,
		  user_agent,
                  accept,
                  accept_language,
                  accept_encoding,
                  accept_charset
                 ],
                 ['keep-alive',
		  'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.162 Safari/535.19',
		  '*/*',
                  'en-us,en;q=0.8',
                  'gzip,deflate,sdch',
                  'ISO-8859-1,utf-8;q=0.7,*;q=0.3'
                  ], Headers).

'chrome_browser_swipl_v?'(_, headers, Headers) :-

   obj_construct(http_experimental_1_0_request_headers_v,
                 [connection,
		  user_agent,
                  accept,
                  accept_language,
                  accept_encoding,
                  accept_charset
                 ],
                 ['keep-alive',
		  'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.19 (KHTML, like Gecko) Chrome/18.0.1025.162 Safari/535.19',
		  '*/*',
                  'en-us,en;q=0.8',
                  'deflate',
                  'ISO-8859-1,utf-8;q=0.7,*;q=0.3'
                  ], Headers).
