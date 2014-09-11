%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%
%  The object for representing set of cookies.

:- module(cookies_man, 
          [ store_cookies/5,  % +DB_Key, +Domain, +Path,
                              % +Headers0, -Headers
            
            retrieve_cookies_headers_/5, % deprecated
            retrieve_cookies_headers/5,
            new_cookie_db_key/1
          ]).

:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(http/v/cookie_v)).
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
    
        
% store_cookies(+DB_Key, +Domain, +Path,
%               +Headers0, -Headers)
%
% It is like in-browser cookies storing
% Extract all set_cookie/1 preds
%

store_cookies(DB_Key, Domain, Path, Headers0, Headers) :-

   Ctx = context(store_cookies/5, _),
   check_db_key(DB_Key, Ctx),
   must_be(atom, Domain),
   must_be(atom, Path),
   must_be(nonvar, Headers0),
   
   extract_cookies(Headers0, Headers, Cookies),
   maplist(store_cookie(DB_Key, Domain, Path), Cookies).

% store_cookie(+DB_Key, +Domain, +Path, +Set_Cookie)

store_cookie(DB_Key, Domain, Path, Set_Cookie) :-

   Ctx = context(extract_cookie/4, _),
   check_db_key(DB_Key, Ctx),

   (   set_cookie_obj(Set_Cookie, Domain, Path, Cookie_Obj0)
   ->
       store_cookie(DB_Key, Cookie_Obj0, _)
   ;
       write_log([DB_Key, ': ignore ', Set_Cookie],
                 [logger(cookies), lf(1, before), lf(1)])
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
       db_erase(Old_Cookie_Obj),
       % TODO only in a case of logging:
       obj_rebase((db_object_v -> object_v),
                  Old_Cookie_Obj, Old_Cookie_Obj1),
       obj_diff(Old_Cookie_Obj1, Cookie_Obj1, Diff_List),
       obj_field(Cookie_Obj1, name, Cookie_Name1),
       write_log([DB_Key, ': u  ', Cookie_Name1, Diff_List],
                 [logger(cookies), lf(1, before), lf(1)])
   ;
       Cookie_Obj1 = Cookie_Obj0,
       obj_field(Cookie_Obj1, set_cookie, Set_Cookie1),
       write_log([DB_Key, ': <- ', Set_Cookie1],
                 [logger(cookies), lf(1, before), lf(1)])
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
                        _),
       must_be(oneof([true, false]), Host_Only), % check DB consistency
       Host_Only = true
   ;
       % TODO optimize the match ('like'?)
       named_args_unify(DB_Key, _,
                        [domain, path, host_only, set_cookie],
                        [Domain, Path, Host_Only, Set_Cookie],
                        _),
       must_be(oneof([true, false]), Host_Only), % check DB consistency
       Host_Only = false,
       domain_match(Host_Can, Domain)
   ),

   path_match(Uri_Path, Path),

   write_log([DB_Key, ': -> ', Set_Cookie],
             [logger(cookies), lf(1, before), lf(1)]).
   % securiy_only flag : TODO
   % http_only flag : TODO
       
   % p.2 (sorting) : TODO
   % p.3 (last access time) : TODO

       
  
%% retrieve_cookies_headers_(+DB_Key, ?Domain, ?Path, -Headers0, ?Headers)
%
% Get cookies headers for request to Domain/Path as a difference list
%
% Deprecated
% @deprecated

retrieve_cookies_headers_(DB_Key, Domain, Path, Headers0, Headers) :-

   atom(DB_Key),

   write_log(['Extract cookies from', DB_Key, 'for',
              Domain, '/', Path],
             [logger(cookies), lf(1, before), lf(1)]),
   
   findall(Set_Cookie,
           load_cookie(DB_Key, Domain, Path, Set_Cookie),
           Cookies),

   (  Cookies = []
   -> Headers0 = Headers
   ;  phrase(cookies_headers(Cookies), Value_Codes, []),
      atom_codes(Value, Value_Codes),
      Headers0 = [request_header('Cookie' = Value)|Headers]
   ).
  
%% retrieve_cookies_headers_(+DB_Key, ?Domain, ?Path, -Headers0, ?Headers)
%
% Get cookies headers for request to Domain/Path as a difference list

retrieve_cookies_headers(DB_Key, Domain, Path, Headers0, Headers) :-

   atom(DB_Key),

   write_log(['Extract cookies from', DB_Key, 'for',
              Domain, '/', Path],
             [logger(cookies), lf(1, before), lf(1)]),
   
   findall(Set_Cookie,
           load_cookie(DB_Key, Domain, Path, Set_Cookie),
           Cookies),

   (  Cookies = []
   -> Headers0 = Headers
   ;  phrase(cookies_headers(Cookies), Value_Codes, []),
      atom_codes(Value, Value_Codes),
      Headers0 = [cookie(Value)|Headers]
   ).
  

%
% Get http response headers and extract all cookies
%
% extract_cookies(+Headers0, -Headers, -Cookies)
%

extract_cookies(Headers0, Headers, Cookies) :-

   extract_cookies(Headers0, Headers, [], Cookies).

extract_cookies([], [], Cookies, Cookies) :- !.

extract_cookies([set_cookie(Cookie)|T0], T,
                Cookies0, [Cookie|Cookies]) :-

   !, extract_cookies(T0, T, Cookies0, Cookies).

extract_cookies([H0|T0], [H0|T], Cookies0, Cookies) :-

   extract_cookies(T0, T, Cookies0, Cookies).
   
extract_cookies(Headers, Cookies) :-

    %%! _Params is not checked (e.g., domain)
    findall(Cookie,
            member(set_cookie(Cookie), Headers),
            Cookies).

cookies_headers([]) -->

   [], !.

cookies_headers([Cookie]) -->

   !, cookies_header(Cookie).
   
cookies_headers([Cookie|Tail]) -->

   cookies_header(Cookie),
   "; ",
   cookies_headers(Tail).

cookies_header(set_cookie(Name, Value, _)) -->

   {  atom_codes(Name, Name_Codes),
      atom_codes(Value, Value_Codes)
   },
   Name_Codes, "=", Value_Codes.
