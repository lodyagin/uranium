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
          [canonicalize_host/2, % +Host, -Host_Can
           domain_match/2,      % +Subject_Domain, +Match_To_Domain
           path_match/2,        % +Subject_Path, +Match_To_Path
           set_cookie_obj/4,    % +Set_Cookie, +Request_Host, +Uri_Path,
                                % -Obj
           prolog:message//1
          ]).

:- use_module(u(v)).
:- use_module(u(parser/general/dcg_entities)).
:- use_module(u(internal/check_arg)).

:- multifile prolog:message//1.

new_class(cookie_v, object_v,
          [%request_host,
	   set_cookie,
           name, value, domain, path, expires, secure_only, http_only,
           creation_time, last_access_time, persistent, host_only],
          [domain, path, name]).

new_class(exact_domain_cookie_v, cookie_v, []).

new_class(subdomains_cookie_v, cookie_v, []).

% set_cookie_obj(+Set_Cookie, +Request_Host, +Uri_Path, -Obj)
%
% set_cookie/3 -> cookie_v constructor

set_cookie_obj(Set_Cookie, Request_Host, Uri_Path, Obj) :-

   must_be(set_cookie/3, Set_Cookie),
   must_be(atom, Request_Host),
   must_be(atom, Uri_Path),

   Set_Cookie = set_cookie(_, _, List),

   canonicalize_host(Request_Host, Request_Host_Can),
   uri_default_path(Uri_Path, Default_Path),
   get_time(Time_Stamp),
   stamp_date_time(Time_Stamp, Current_Date_Time, local),

   % Set default values
   % <NB> do not set the default path (rfc 6265, 5.1.4)
   obj_construct(cookie_v,
                 [%request_host,
		  set_cookie,
                  persistent,
                  domain,
                  host_only,
                  path,
                  secure_only,
                  http_only,
                  creation_time,
                  last_access_time
                 ],
                 [%Request_Host_Can,
                  Set_Cookie,
                  false,   % rfc 6265, 5.3, 3
                  Request_Host_Can,  % 5.3, 6
                  true,    % rfc 6265, 5.3, 6
                  Default_Path,      % 5.3, 7
                  false,   % rfc 6265, 5.3, 8
                  false,   % rfc 6265, 5.3, 9
                  Current_Date_Time, % 5.3, 2
                  Current_Date_Time  % 5.3, 2
                  ],
                 Obj0),

   parse_attr_list(List, Obj0, Obj).

parse_attr_list([], Obj, Obj) :- !.

parse_attr_list([Attr0=Val|Tail], Obj0, Obj) :-

   downcase_atom(Attr0, Attr),
   parse_attr(Attr, Val, Obj0, Obj1),
   parse_attr_list(Tail, Obj1, Obj).

%parse_attr(expires, Date_Str, Obj0, Obj) :- !,

%   parse_date(Date_Str, Date), % can fail, see rfc 6265, 5.2.1
%   obj_rewrite(Obj0, [expires], _, [Date]. Obj).
%   ! check rfc 6265, 5.3, 3 also

%parse_attr('max_age', Delta_Seconds_Str, Obj0, Obj) :- !,
%   ! check rfc 6265, 5.3, 3 also

parse_attr(domain, Domain0, Obj0, Obj) :- !,

   atom(Domain0), Domain0 \= '', % according to rfc 6265, 5.2.3
   normalize_domain(Domain0, Domain),
   obj_field(Obj0, request_host, Request_Host_Can),
   domain_match(Domain, Request_Host_Can), % 5.1.3
   obj_rewrite(Obj0, [host_only, domain], _, [false, Domain], Obj).

parse_attr(path, Path, Obj0, Obj) :- !,

   check_path(Path),
   obj_rewrite(Obj0, [path], _, [Path], Obj).

parse_attr(secure, _, Obj0, Obj) :- !,

  obj_rewrite(Obj0, [secure_only], _, [true], Obj).

parse_attr(httponly, _, Obj0, Obj) :- !,

  % FIXME rfc 6265 5.3, 10
  % (when cookie was received from a "non-HTTP" API)
  obj_rewrite(Obj0, [http_only], _, [true], Obj).

parse_attr(Attr, Val, Obj, Obj) :-

   print_message(warning, ignore_cookie_attribute(Attr, Val, Obj)).

% 5.1.3
domain_match(Subj_Domain, Match_To_Domain) :-
   downcase_atom(Subj_Domain, S),
   downcase_atom(Match_To_Domain, M),
   domain_match_lowercase(S, M).

domain_match_lowercase(Dom, Dom) :- !.

domain_match_lowercase(S, M) :-
   atom_codes(M, MC),
   \+ phrase(ipv4(_), MC), % FIXME ipv4 only
   atom_concat(A, S, M),
   atom_concat(_, '.', A).
   

% 5.1.4
uri_default_path('', '/') :- !.

uri_default_path('/', '/') :- !.

uri_default_path(Uri_Path, '/') :-
   var(Uri_Path), !.

uri_default_path(Uri_Path, '/') :-
   \+ atom_concat('/', _, Uri_Path), !.

uri_default_path(Uri_Path, Default_Path) :-
   atom_concat(Default_Path, '/', Uri_Path), !.

uri_default_path(Path, Path).


% 5.1.3
path_match(Path, Path) :- !.

path_match(S, M) :-
   atom_concat(M, B, S),
   ( atom_concat(_, '/', M)
   ; atom_concat('/', _, B)
   ), !.

% 5.2.3
normalize_domain(Domain0, Domain) :-
   atom_concat('.', Domain, Domain0), !.

normalize_domain(Domain, Domain).

canonicalize_host(Host, Host_Can) :-

   % FIXME: rfc 5890 (IDNA-strings)
   Host_Can = Host.

% 5.2.4
check_path(Path) :-
   atom(Path), Path \= '',
   atom_concat('/', _, Path). 


prolog:message(ignore_cookie_attribute(Attr, Val, Obj)) :-

   ['The cookie attribute ~a=~a is ignored for ~p' -
   [Attr, Val, Obj]].


