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

:- module(cookies_man, 
          [ store_cookies/4,  %+DB_Key, +Headers, +Domain,
                              %+Path
            retrieve_cookies_headers/4,
            new_cookie_db_key/1,
            extract_cookies/2,
            cookies_headers/2
          ]).

:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(html/v/cookie_v)).
:- use_module(u(logging)).
:- use_module(u(internal/check_arg)).

%
% Generate a new cookie db key
%
% new_cookie_db_key(-New_DB_Key)
%

new_cookie_db_key(New_DB_Key) :-

  repeat, % for multithreading safety
  flag(cookies_man_db_key, Old, Old),
  succ(Old, New),
  flag(cookies_man_db_key, Old, New), % can fail when multithread
  !,

  atom_concat(cookie_db, New, New_DB_Key).
    
        
% store_cookies(+DB_Key, +Headers, +Domain, +Path)
%
% It is like in-browser cookies storing
%

store_cookies(DB_Key, Headers, Domain, Path) :-

   must_be(nonvar, Headers),
   
   extract_cookies(Headers, Cookies),
   maplist(store_cookie(DB_Key, Domain, Path), Cookies).

% store_cookie(+DB_Key, +Domain, +Path, +Set_Cookie)

store_cookie(DB_Key, Domain, Path, Set_Cookie) :-

   Ctx = context(extract_cookie/4, _),
   check_db_key(DB_Key, Ctx),

   (   set_cookie_obj(Set_Cookie, Domain, Path, Cookie_Obj0)
   ->
       store_cookie(DB_Key, Cookie_Obj0, Cookie_Obj),
       write_log(['New cookie in', DB_Key, ':', Cookie_Obj],
                 [logger(cookies), lf(1, before), lf(1)])
   ;
       true
   ).

% rfc 6265, 5.3
store_cookie(DB_Key, Cookie_Obj0, Cookie_Obj) :-

   % p.1 is not implemented
   % p.2,3,4 are done in cookie_v:set_cookie_obj
   % p.5 (public suffix) : TODO
   % p.6,7,8,9 is done in cookie_v:set_cookie_obj
   % p.10 : TODO

   % p.11
   % p.11.2 : TODO

   % p.11.3
   named_args_unify(Cookie_Obj0,
                    [name, domain, path], [Name, Domain, Path]),
   
   (   named_args_unify(DB_Key, _,
                        [name, domain, path, creation_time],
                        [Name, Domain, Path, Creation_Time],
                        Old_Cookie_Obj)
   ->
       obj_rewrite(Cookie_Obj0, [creation_time], _, [Creation_Time],
                   Cookie_Obj1),
       db_erase(Old_Cookie_Obj)
   ;
       Cookie_Obj1 = Cookie_Obj0
   ),
   
   % p.11.4
   db_put_object(DB_Key, Cookie_Obj1, Cookie_Obj).

% load_cookie(+DB_Key, +Request_Host, +Uri_Path, -Set_Cookie) is nondet
% rfc 6265, 5.4
%
load_cookie(DB_Key, Request_Host, Uri_Path, Set_Cookie) :-

  canonicalize_host(Request_Host, Host_Can),

   % p.1
   (   named_args_unify(DB_Key, _,
                        [domain, path, host_only, set_cookie],
                        [Host_Can, Path, Host_Only, Set_Cookie],
                        Obj1),
       must_be(oneof([true, false]), Host_Only), % check DB consistency
       Host_Only = true
   ;
       % TODO optimize the match ('like'?)
       named_args_unify(DB_Key, _,
                        [domain, path, host_only, set_cookie],
                        [Domain, Path, Host_Only, Set_Cookie],
                        Obj1),
       must_be(oneof([true, false]), Host_Only), % check DB consistency
       Host_Only = false,
       domain_match(Host_Can, Domain)
   ),

   path_match(Uri_Path, Path).

   % securiy_only flag : TODO
   % http_only flag : TODO
       
   % p.2 (sorting) : TODO
   % p.3 (last access time) : TODO

       
  
%
% Get cookies headers before http request
% 
% retrieve_cookies_headers(+DB_Key, ?Domain, ?Path, -Headers)

retrieve_cookies_headers(DB_Key, Domain, Path, Headers) :-

  atom(DB_Key),
  findall(Set_Cookie,
          load_cookie(DB_Key, Domain, Path, Set_Cookie),
          Cookies),

  write_log(['Extract cookies from', DB_Key, 'for',
             Domain, '/', Path, ':',
             Cookies],
            [logger(cookies), lf(1, before), lf(1)]),
             
  cookies_headers(Cookies, Headers).
  

%
% Get http response headers and return all cookies
%
% extract_cookies(+Headers, -Cookies)
%

extract_cookies(Headers, Cookies) :-

    %%! _Params is not checked (e.g., domain)
    findall(Cookie,
            member(set_cookie(Cookie), Headers),
            Cookies).

%% Return request headers with stored cookies
%%! Site is not checked
%
% cookies_headers(+Cookies, -Headers)
%

cookies_headers(Cookies, Headers) :-
    findall(request_header('Cookie' = Hdr),
            (member(set_cookie(Name, Value, _), Cookies),
             format(atom(Hdr), '~a=~a', [Name, Value])
              ),
             Headers
             ).

