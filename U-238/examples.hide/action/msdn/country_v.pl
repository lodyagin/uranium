% This file is a part of Uranium, a general-purpose functional test platform.
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
% 
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% -------------------------------------------------------------------------------
%

:- module(country_v, []).

new_class(country_v, object_v,
          [country, country_strings,
           parsed % is bound if country_strings is parsed as list
          ]
         ).
