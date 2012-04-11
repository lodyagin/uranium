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


:- module(http_user_v, [new_http_user/1]).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(http/cookies_man)).


% it is a 'free' internet user
new_class(http_user_v, object_v,
          [cookies_db_key : cookies_db,
           current_url
          ]).

copy(http_user_v, From, To) :-

   obj_reset_fields([cookie_db_key], From, To),

   obj_field(From, cookie_db_key, Old_DB_Key),
   (   ground(Old_DB_Key)
   ->  use_module(u(http/cookies_man)),
       new_cookie_db_key(New_DB_Key),
       use_module(u(vd)),
       db_copy(Old_DB_Key, New_DB_Key),
       obj_field(To, cookie_db_key, New_DB_Key)
   ;   true
   ).


new_http_user(User) :-

  new_cookie_db_key(Cookies_DB),
  obj_construct(http_user_v, [cookie_db_key], [Cookies_DB], User).


typedef(cookies_db, [pretty_print - cookies_db_pretty_print]).

cookies_db_pretty_print(_, Value, Options) :-

  % FIXME do not use Stream

  dump_db(Options, Value).















