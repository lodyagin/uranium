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

:- use_module(u(v)).
:- use_module(u(ur_recorded_db)).
:- use_module(u(html/v/cookie_v)).
:- use_module(u(logging)).

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
    
        
%
% It is like in-browser cookies storing
%

store_cookies(DB_Key, Headers, Domain, Path) :-

   extract_cookies(Headers, Cookies),
   maplist(extract_cookie(DB_Key, Domain, Path), Cookies).

extract_cookie(DB_Key, Domain, Path, Set_Cookie) :-

   cookie_object(Domain, Path, Cookie_Obj, Set_Cookie),
   db_put_object(DB_Key, Cookie_Obj, [overwrite]),
   write_log(['New cookie in', DB_Key, ':', Cookie_Obj],
             [logger(cookies), lf(1, before), lf(1)]).

%
% Get cookies headers before http request
% 
% retrieve_cookies_headers(+DB_Key, ?Domain, ?Path, -Headers)

retrieve_cookies_headers(DB_Key, Domain, Path, Headers) :-

  atom(DB_Key),
  findall(Cookie_Obj,
          unify_cookie(DB_Key, Domain, Path, Cookie_Obj),
          Cookies_Obj),

  maplist(cookie_object(_, _), Cookies_Obj, Cookies),

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

% cookie_object([+]Visited_Domain, [+]Visited_Path, ?Cookie_Obj, ?Cookie_Header)
%
% see rfc 6265

cookie_object(Visited_Domain, Visited_Path,
              Cookie_Obj, set_cookie(Name, Value, List)
              ) :-

   ( ground(List) -> List2 = List ; true ),
        
   ignore(memberchk(expires=Expires, List2)),
        
   (   memberchk(path=Path, List2)
   ->  true
   ;   Path = Visited_Path
   ),

   (   memberchk(domain=Domain, List2)
   ->  Class = subdomains_cookie_v
   ;   Domain = Visited_Domain,
       Class = exact_domain_cookie_v
   ),

   ignore(memberchk(secure=Secure, List2)),
   ignore(memberchk(httponly=Httponly, List2)),
   
   obj_construct(Class,
                 [domain, path, name, value, expires, secure, httponly],
                 [Domain, Path, Name, Value, Expires, Secure, Httponly],
                 Cookie_Obj),

   include(ground, List2, List), !.


