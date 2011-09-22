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

:- module(cookie_v,
          [unify_cookie/4
           ]).

:- use_module(library(ul_objects)).

:- module_transparent unify_cookie/4.

%
% Retrieve cookies from db by domain and path
% by RFC 6265 rules
%
% unify_cookie(+DB_Key, ?Domain, ?Path, ?Cookie)
%
% FIXME! need to check the secure attribute

unify_cookie(DB_Key, Domain, Path, Cookie) :-

  atomic(DB_Key),
  named_args_unify(DB_Key, Class,
                  [domain, path], [Cookie_Domain, Cookie_Path],
                  Cookie),
  
  (  ( Class = subdomains_cookie_v
     ; class_descendant(subdomains_cookie_v, Class)
     )
  ->
     % apply to subdomains also
     (   atom_concat('.', Top_Domain, Cookie_Domain) % '.' is optional
     ->  true
     ;   Top_Domain = Cookie_Domain
     ),
     (   Domain = Top_Domain
     ->  true
     ;   atom_concat('.', Top_Domain, Top_Domain_With_Dot),
         atom_concat(_, Top_Domain_With_Dot, Domain)
     )
  ;
     % exact domain match
     Domain = Cookie_Domain
  ),

  (  atom_concat(_, '/', Cookie_Path)
  -> CP = Cookie_Path
  ;  atom_concat(Cookie_Path, '/', CP)
  ),
  atom_concat(CP, _, Path).

new_class(cookie_v, object_v,
          [name, value, domain, path, expires, secure, httponly],
          [domain, path, name]).

% FIXME must use 4 args to be loaded after cookie_v
new_class(exact_domain_cookie_v, cookie_v, [], []).

new_class(subdomains_cookie_v, cookie_v, [], []).

