% -*- fill-column: 65; -*-
% _____________________________________________________________
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% _____________________________________________________________

:- module(ur_url,
          [url_normalize/2,  % +Url0, -Url
           url_normalize/3   % +Url0, +Base_Url, -Url
           ]
         ).

:- use_module(library(uri)).

url_normalize(Url0, Url) :-

   uri_normalized(Url0, Url1),
   eat_tail_slashes(Url1, Url).

url_normalize(Url0, Base_Url, Url) :-

   uri_normalized(Url0, Base_Url, Url1),
   eat_tail_slashes(Url1, Url).

eat_tail_slashes(Url0, Url) :-

   (   atom_concat(Url1, '/', Url0)
   ->  eat_tail_slashes(Url1, Url)
   ;   Url0 = Url
   ).



