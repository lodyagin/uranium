% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose
% functional test platform.
%
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it
% and/or modify it under the terms of the GNU Lesser
% General Public License as published by the Free Software
% Foundation; either version 2.1 of the License, or (at
% your option) any later version.
%
% This library is distributed in the hope that it will be
% useful, but WITHOUT ANY WARRANTY; without even the
% implied warranty of MERCHANTABILITY or FITNESS FOR A
% PARTICULAR PURPOSE.  See the GNU Lesser General Public
% License for more details.
%
% You should have received a copy of the GNU Lesser
% General Public License along with this library; if not,
% write to the Free Software Foundation, Inc., 51 Franklin
% Street, Fifth Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(rfc2616,
          [response//1
           ]).

/** <module> RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1

  TODO rfc 2047 (MIME)
*/

:- use_module(u(v)).
:- use_module(rfc822).
:- use_module(v(http_headers_v)).

/*
  2.2 Basic Rules

       OCTET          = <any 8-bit sequence of data>
       CHAR           = <any US-ASCII character (octets 0 - 127)>
       UPALPHA        = <any US-ASCII uppercase letter "A".."Z">
       LOALPHA        = <any US-ASCII lowercase letter "a".."z">
       ALPHA          = UPALPHA | LOALPHA
       DIGIT          = <any US-ASCII digit "0".."9">
       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>
       CR             = <US-ASCII CR, carriage return (13)>
       LF             = <US-ASCII LF, linefeed (10)>
       SP             = <US-ASCII SP, space (32)>
       HT             = <US-ASCII HT, horizontal-tab (9)>
       <">            = <US-ASCII double-quote mark (34)>

       CRLF           = CR LF

       LWS            = [CRLF] 1*( SP | HT )

       TEXT           = <any OCTET except CTLs,
                        but including LWS>

       HEX            = "A" | "B" | "C" | "D" | "E" | "F"
                      | "a" | "b" | "c" | "d" | "e" | "f" | DIGIT

       token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT

       comment        = "(" *( ctext | quoted-pair | comment ) ")"
       ctext          = <any TEXT excluding "(" and ")">

       quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
       qdtext         = <any TEXT except <">>

       quoted-pair    = "\" CHAR
*/

char(C) --> [C], { between(0, 127, C) }.

sp --> " ".

crlf --> "\r\n".

/*
  3.1 HTTP Version

         HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
*/
http_version(version(Major, Minor)) -->

   "HTTP/", number(Major), ".", number(Minor).

/*
  4.2 Message Headers

  ... The field-content does not include any leading or trailing LWS:
   linear white space occurring before the first non-whitespace
   character of the field-value or after the last non-whitespace
   character of the field-value. Such leading or trailing LWS MAY be
   removed without changing the semantics of the field value.

   *TODO*
   Any LWS that occurs between field-content MAY be replaced with
   a single SP before interpreting the field value or forwarding
   the message downstream.
*/

% It is implemented in http_headers_v

/*
6 Response

   After receiving and interpreting a request message, a server responds
   with an HTTP response message.

       Response      = Status-Line               ; Section 6.1
                       *(( general-header        ; Section 4.5
                        | response-header        ; Section 6.2
                        | entity-header ) CRLF)  ; Section 7.1
                       CRLF
                       [ message-body ]          ; Section 7.2
  */

% doesn't check type of headers, but can return an http_headers_v object
% derived from http_invalid_headers_v (see field `headers` of Response).
%
% It stops after reading crlf (do not read an entity body)
response(Response) -->

   status_line(HTTP_Version, Status_Code, Reason_Phrase),
   headers(Headers),
   crlf,
   {
     obj_construct(http_response_v,
                   [http_version, status_code, reason_phrase, headers],
                   [HTTP_Version, Status_Code, Reason_Phrase, Headers],
                   Response)
   }.

headers(Headers_Obj) -->

   headers_list(Headers_List, []),
   { http_headers_list_obj(Headers_List, Headers_Obj) }.

headers_list([Name=Value|Headers0], Headers) -->

   field(Name, _, Body_Tokens), !,
   { body_value(Body_Tokens, Value) },
   headers_list(Headers0, Headers).

headers_list(Headers, Headers) --> [].

body_value(Tokens, Value) :-

   body_value(Tokens, _, L2),
   concat_atom(L2, ' ', Value).

body_value([], _, []) :- !.

body_value([' '], _, []) :- !.

body_value([' '|T], first, T2) :- !,

   body_value(T, t, T2).

body_value([Token|L], _, [Token|L2]) :-

   body_value(L, t, L2).

/*
6.1 Status-Line

       Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF

*/

status_line(HTTP_Version, Status_Code, Reason_Phrase) -->

   http_version(HTTP_Version), sp,
   status_code(Status_Code), sp,
   reason_phrase(Reason_Phrase), crlf.

status_code(Status_Code) -->

   { L = [D1, D2, D3] },
   digit(D1), digit(D2), digit(D3),
   { number_codes(Status_Code, L)}.

reason_phrase(Atom) -->

   text_without([0'\n, 0'\r], Codes, []),
   { atom_codes(Atom, Codes) }.


number(N) -->

   digit(C0), digits(C), { number_codes(N, [C0|C]) }.

digit(C) -->
	char(C),
	{ code_type(C, digit)
	}.

digits([H|T]) -->
	digit(H), !,
	digits(T).
digits([]) -->
	[].


%	Take as many tokens from the input until the next token appears in
%	End. End itself is left on the input.

text_without(Not, [C|Str0], Str) -->

   char(C),
   { \+ memberchk(C, Not)
   }, !,
   text_without(Not, Str0, Str).

text_without(_, Str, Str) -->

   [].

