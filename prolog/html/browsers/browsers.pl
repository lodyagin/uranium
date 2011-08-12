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

%
% Contains sample header definitions for various browsers
%

:- module(browsers, [browser_headers/2]).

browser_headers(iceweasel,

  [user_agent('Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.2.17) Gecko/20110430 Iceweasel/3.6.17 (like Firefox/3.6.17)'),
   request_header('Accept'='text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'),
   request_header('Accept-Language'='en-us,en;q=0.5'),
   request_header('Accept-Encoding'='gzip,deflate'),
   request_header('Accept-Charset'='ISO-8859-1,utf-8;q=0.7,*;q=0.7')]
) :- !.