% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
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


:- module(ur_http_client,
          [http_open/4,
           http_response/4,
           http_request/2
           ]).

/** <module> Uranium http low-level client.
*/

:- use_module(library(uri)).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(u(v)).
:- use_module(v(http_headers_v)).
:- use_module(u(internet/common_internet_data)).
:- use_module(u(stream/stream_input)).
:- use_module(u(internet/rfc2616)).


:- predicate_options(http_open/4, 2,
		     [ http_version(nonneg, nonneg), % default is 1.1
                       method(oneof([get,head,post])), % default is `get`
		       timeout(number) % default is `ifinite`
		     ]).

%% http_open(URL, Options, Headers_In, Stream) is semidet.
%
%  Sends HTTP request to URL with Headers_In. Return the
%  streampair. User must read the response.
%
%  Guarantee that the socket connection is either (good) open or
%  closed after the call.
%
%  @param Options
%   * http_version(Major, Minor)
%   * method - get, head, post (get is the default)
%   * timeout(Seconds)  possible value is `infinite`
%
%  @prarm Headers_In headers. If the `host` header is missing it
%  will be generated from URL by rfc2616 rules.
%
%  @param Stream - the streampair for both input and output.

http_open(URL, Options, Headers_In, Stream) :-

   Ctx = context(http_open/4),
   (  atom(URL)
   -> parse_url_ex(URL, Parts)
   ;  Parts = URL
   ),
   http_open_parts(Parts, Options, Headers_In, Stream, Ctx).

http_open_parts(Parts, Options, Headers_In0, Stream, Ctx) :-

   parts_request_uri(Parts, Request_URI),
   parts_host_port(Parts, Host, Port, Host_Port),
   
   % it is rec acc to rfc 2616, 14.23
   add_host_header(Host_Port, Headers_In0, Headers_In),
   open_socket(Host:Port, In, Out, Options),
   send_request(In, Out, Request_URI, Headers_In, Options, Ctx),
   stream_pair(Stream, In, Out).

add_host_header(_, Headers, Headers) :-

   obj_field(Headers, fail, host, Host),
   nonvar(Host), !.

add_host_header(Host_Port, Headers0, Headers) :-

   format(atom(Host_Port_Atom), '~w', [Host_Port]),
   http_headers_list_obj(Headers_List, Headers0),
   http_headers_list_obj([host(Host_Port_Atom)|Headers_List],
                         Headers).

parts_host_port(Parts, Host, Port, Host_Port) :-

   memberchk(host(Host), Parts),
   parts_scheme(Parts, Scheme),
   default_port(Scheme, Default_Port),
   url_part(port(Port), Parts, Default_Port),
   host_and_port(Host, Default_Port, Port, Host_Port).
   

send_request(In, Out, Request_URI, Headers_In, Options, Ctx) :-

	(   catch(guarded_send_request(Out, Request_URI,
                                       Headers_In,
                                       Options, Ctx),
		  E, true)
	->  (   var(E)
	    ->	true
	    ;	force_close(In, Out),
		throw(E)
	    )
	;   force_close(In, Out),
	    fail
	).

guarded_send_request(Out, Request_URI, Headers_In, Options, Ctx) :-

   select_option(method(Method), Options, Options1, get),
   (  map_method(Method, Method_Name)
   -> true
   ;  throw(error(domain_error(http_method, Method), Ctx))
   ),
   select_option(http_version(version(Major, Minor)), Options1, _,
                 version(1, 1)),
   format(Out, '~a ~a HTTP/~d.~d\r\n',
          [Method_Name, Request_URI, Major, Minor]),
   send_headers(Out, Headers_In),
   write(Out, '\r\n'),
   flush_output(Out).

force_close(S1, S2) :-
	close(S1, [force(true)]),
	close(S2, [force(true)]).

host_and_port(Host, DefPort, DefPort, Host) :- !.
host_and_port(Host, _,       Port,    Host:Port).

map_method(get,  'GET').
map_method(head, 'HEAD').
map_method(post, 'POST').

%	open_socket(+Address, -In, -Out, +Options) is det.
%
%	Create and connect a client socket to Address.  Options
%
%	    * timeout(+Timeout)
%	    Sets timeout on the stream, *after* connecting the
%	    socket.
%
%       This predicate is from SWI-Prolog standard library.
%       @author Jan Wielemaker
%       @copyrigth 2008-2011, University of Amsterdam

open_socket(Address, In, Out, Options) :-

   debug(http(open), 'http_open: Connecting to ~p ...', [Address]),
   tcp_socket(Socket),
   catch(tcp_connect(Socket, Address, In, Out),
         E,
         (	  tcp_close_socket(Socket),
		  throw(E)
         )),
   debug(http(open), '\tok ~p --> ~p', [In, Out]),
   set_stream(In, record_position(false)),
   (   memberchk(timeout(Timeout), Options)
   ->  set_stream(In, timeout(Timeout))
   ;   true
   ).



%%	parse_url_ex(+URL, -Parts)
%
%	Parts:  Schema,  Host,  Port,    User:Password,  RequestURI  (no
%	fragment).
%
%       This predicate is from SWI-Prolog standard library.
%       @author Jan Wielemaker
%       @copyrigth 2008-2011, University of Amsterdam

parse_url_ex(URL, [uri(URL)|Parts]) :-
	uri_components(URL, Components),
	phrase(components(Components), Parts),
	(   memberchk(host(_), Parts)
	->  true
	;   domain_error(url, URL)
	).

components(Components) -->
	uri_scheme(Components),
	uri_authority(Components),
	uri_request_uri(Components).

uri_scheme(Components) -->
	{ uri_data(scheme, Components, Scheme), nonvar(Scheme) }, !,
	[ scheme(Scheme)
	].
uri_scheme(_) --> [].

uri_authority(Components) -->
	{ uri_data(authority, Components, Auth), nonvar(Auth), !,
	  uri_authority_components(Auth, Data)
	},
	[ authority(Auth) ],
	auth_field(user, Data),
	auth_field(password, Data),
	auth_field(host, Data),
	auth_field(port, Data).
uri_authority(_) --> [].

auth_field(Field, Data) -->
	{ uri_authority_data(Field, Data, EncValue), nonvar(EncValue), !,
	  (   atom(EncValue)
	  ->  uri_encoded(query_value, Value, EncValue)
	  ;   Value = EncValue
	  ),
	  Part =.. [Field,Value]
	},
	[ Part ].
auth_field(_, _) --> [].

uri_request_uri(Components) -->
	{ uri_data(path, Components, Path0),
	  uri_data(search, Components, Search),
	  (   Path0 == ''
	  ->  Path = (/)
	  ;   Path = Path0
	  ),
	  uri_data(path, Components2, Path),
	  uri_data(search, Components2, Search),
	  uri_components(RequestURI, Components2)
	},
	[ request_uri(RequestURI)
	].

%%	parts_scheme(+Parts, -Scheme) is det.
%%	parts_uri(+Parts, -URI) is det.
%%	parts_request_uri(+Parts, -RequestURI) is det.
%%	parts_search(+Parts, -Search) is det.
%%	parts_authority(+Parts, -Authority) is semidet.

parts_scheme(Parts, Scheme) :-
	url_part(scheme(Scheme), Parts), !.
parts_scheme(Parts, Scheme) :-		% compatibility with library(url)
	url_part(protocol(Scheme), Parts), !.
parts_scheme(_, http).

parts_authority(Parts, Auth) :-
	url_part(authority(Auth), Parts), !.
parts_authority(Parts, Auth) :-
	url_part(host(Host), Parts, _),
	url_part(port(Port), Parts, _),
	url_part(user(User), Parts, _),
	url_part(password(Password), Parts, _),
	uri_authority_components(Auth,
				 uri_authority(User, Password, Host, Port)).

parts_request_uri(Parts, RequestURI) :-
	memberchk(request_uri(RequestURI), Parts), !.
parts_request_uri(Parts, RequestURI) :-
	url_part(path(Path), Parts, /),
	ignore(parts_search(Parts, Search)),
	uri_data(path, Data, Path),
	uri_data(search, Data, Search),
	uri_components(RequestURI, Data).

parts_search(Parts, Search) :-
	memberchk(query_string(Search), Parts), !.
parts_search(Parts, Search) :-
	memberchk(search(Fields), Parts), !,
	uri_query_components(Search, Fields).


parts_uri(Parts, URI) :-
	memberchk(uri(URI), Parts), !.
parts_uri(Parts, URI) :-
	parts_scheme(Parts, Scheme),
	ignore(parts_authority(Parts, Auth)),
	parts_request_uri(Parts, RequestURI),
	uri_components(RequestURI, Data),
	uri_data(scheme, Data, Scheme),
	uri_data(authority, Data, Auth),
	uri_components(URI, Data).

url_part(Part, Parts) :-
	Part =.. [Name,Value],
	Gen =.. [Name,RawValue],
	memberchk(Gen, Parts), !,
	Value = RawValue.

url_part(Part, Parts, Default) :-
	Part =.. [Name,Value],
	Gen =.. [Name,RawValue],
	(   memberchk(Gen, Parts)
	->  Value = RawValue
	;   Value = Default
	).


%% http_request(+Stream, +Request) is det.
%
% Request must have these mandatory fields: method,
% request_uri, http_version
http_request(Stream, Request) :-

   obj_unify(Request,
             [method, request_uri, http_version, headers],
             [Method, URI, Version, Headers]),
   upcase_atom(Method, Method_Upcase),
   format(Stream, '~a ~a HTTP/~a\r\n',
          [Method_Upcase, URI, Version]),
   ( var(Headers) -> true
   ; send_headers(Stream, Headers)
   ),
   write(Stream, '\r\n'),
   flush_output(Stream).

%% http_response(+Stream_DB, +Stream, -Response, -Tail) is det.
% Read an http_response_v object Response from Stream.
%
% @param Stream_DB used as a read buffer (DB key).
% @param Tail unread part of server output (after
%  message-body). (Normally it should be []).

http_response(Stream_DB, Stream, Response, Tail) :-

   stream_to_lazy_list(Stream_DB, Stream, stream_buffer_v,
                       _, List),
   phrase(response(Response), List, Tail).
   

