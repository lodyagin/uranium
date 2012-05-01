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

:- module(rfc822,
          [field//2
	   %email_address//1
           ]).

/** <module> RFC 822 - ARPA Internet Text Messages
*/

/*
     3.2.  HEADER FIELD DEFINITIONS
     ...

     field       =  field-name ":" [ field-body ] CRLF

     field-name  =  1*<any CHAR, excluding CTLs, SPACE, and ":">

     field-body  =  field-body-contents
                    [CRLF LWSP-char field-body]

     field-body-contents =
                   <the ASCII characters making up the field-body, as
                    defined in the following sections, and consisting
                    of combinations of atom, quoted-string, and
                    specials tokens, or else consisting of texts>
*/

field(Name, Body) -->

   field_name(Name), ":", field2(Body).

field2(Body) -->

   field_body(Str, []), !, crlf(_),
   { atom_codes(Body, Str) }.

field2('') --> crlf(_).

field_name(Name) -->

   field_name_char(C0),
   field_name_chars(C),
   { atom_codes(Name, [C0|C]) }.

field_name_char(_) --> ( ctl(_) ; " " ; ":" ), !, { fail }.

field_name_char(C) --> [C].

field_name_chars([C0|C]) -->

   field_name_char(C0), !,
   field_name_chars(C).

field_name_chars([]) --> [].

field_body(Str0, Str) -->

   field_body_contents(Str0, Str1), !,
   field_body_continuation(Str1, Str).

field_body_contents(Str0, Str) -->

   field_body_contents2_el(Str0, Str1),
   field_body_contents2(Str1, Str).

field_body_contents2_el(Str0, Str) --> atom(Str0, Str), !.

field_body_contents2_el(Str0, Str) --> quoted_string(Str0, Str), !.

field_body_contents2_el([C|Str], Str) --> specials(C), !.

field_body_contents2_el(Str0, Str) --> text(Str0, Str).

field_body_contents2(Str0, Str) -->

   field_body_contents2_el(Str0, Str1), !,
   field_body_contents2(Str1, Str).

field_body_contents2(Str, Str) --> [].

field_body_continuation([C0|Str0], Str) -->

   crlf(_), lwsp(C0), !,
   field_body(Str0, Str).

field_body_continuation(Str, Str) --> [].

/*
     3.3.  LEXICAL TOKENS

                                                 ; (  Octal, Decimal.)
     CHAR        =  <any ASCII character>        ; (  0-177,  0.-127.)
     ALPHA       =  <any ASCII alphabetic character>
                                                 ; (101-132, 65.- 90.)
                                                 ; (141-172, 97.-122.)
     DIGIT       =  <any ASCII decimal digit>    ; ( 60- 71, 48.- 57.)
     CTL         =  <any ASCII control           ; (  0- 37,  0.- 31.)
                     character and DEL>          ; (    177,     127.)
     CR          =  <ASCII CR, carriage return>  ; (     15,      13.)
     LF          =  <ASCII LF, linefeed>         ; (     12,      10.)
     SPACE       =  <ASCII SP, space>            ; (     40,      32.)
     HTAB        =  <ASCII HT, horizontal-tab>   ; (     11,       9.)
     <">         =  <ASCII quote mark>           ; (     42,      34.)
     CRLF        =  CR LF

     LWSP-char   =  SPACE / HTAB                 ; semantics = SPACE

     linear-white-space =  1*([CRLF] LWSP-char)  ; semantics = SPACE
                                                 ; CRLF => folding

     specials    =  "(" / ")" / "<" / ">" / "@"  ; Must be in quoted-
                 /  "," / ";" / ":" / "\" / <">  ;  string, to use
                 /  "." / "[" / "]"              ;  within a word.

     delimiters  =  specials / linear-white-space / comment
*/

char(C) --> [C], { between(0, 127, C) }.

alpha(C) --> [C], { ( between(65, 90, C); between(97, 122, C) ), ! }.

digit(C) --> [C], { between(48, 57, C) }.

ctl(C) --> [C], { ( between(0, 31, C) ; C = 127 ), ! }.

crlf(Str) --> { Str = "\r\n" }, Str.

htab(C) --> { C = 9 }, [C].

lwsp(C) --> { C = 32 }, [C], !.

lwsp(C) --> htab(C).

linear_white_space([C2|Str0], Str) -->

   crlf(_), lwsp(C2),  % <NB> remove crlf accourding to 3.1.1
   linear_white_space2(Str0, Str).

linear_white_space2(Str0, Str) -->

   linear_white_space(Str0, Str1), !,
   linear_white_space2(Str1, Str).

linear_white_space2(Str, Str) --> [].

specials(C) -->

   [C],
   { memberchk(C, [0'(, 0'), 0'<, 0'>, 0'@, 0',, 0';, 0':,
                   0'\\, 0'", 0'., 0'[, 0'] %"
                  ])
   }.

delimiters([C|Str], Str) --> specials(C), !.

delimiters(Str0, Str) --> linear_white_space(Str0, Str), !.

delimiters(Str0, Str) --> comment(Str0, Str).


/*
     text        =  <any CHAR, including bare    ; => atoms, specials,
                     CR & bare LF, but NOT       ;  comments and
                     including CRLF>             ;  quoted-strings are
                                                 ;  NOT recognized.
*/

text([C0|Str0], Str) -->

   text_char(C0),
   text_chars(Str0, Str).

text_char(C0) -->

   "\r", !,
   (  "\n"
   -> { fail         }  % preventing CRLF
   ;  { C0 = 0'\r %'
      }
   ).

text_char(C) --> [C].

text_chars([C0|Str0], Str) -->

   text_char(C0), !,
   text_chars(Str0, Str).

text_chars(Str, Str) --> [].


%    atom        =  1*<any CHAR except specials, SPACE and CTLs>

atom([C0|Str0], Str) -->

   atom_char(C0),
   atom_chars(Str0, Str).

atom_char(_) -->

   ( specials(_) ; " " ;  ctl(_) ), !, { fail }.

atom_char(C) -->

   char(C).

atom_chars([C0|Str0], Str) -->

   atom_char(C0), !,
   atom_chars(Str0, Str).

atom_chars(Str, Str) --> [].

/*
    quoted-string = <"> *(qtext/quoted-pair) <">; Regular qtext or
                                                 ;   quoted chars.

    qtext       =  <any CHAR excepting <">,     ; => may be folded
                     "\" & CR, and including
                     linear-white-space>
*/

quoted_string(Str0, Str) -->

   "\"", %"
   quoted_string2(Str0, Str),
   "\"". %".

quoted_string2(Str0, Str) -->

   quoted_string2_el(Str0, Str1), !,
   quoted_string2(Str1, Str).

quoted_string2(Str, Str) --> [].

quoted_string2_el(Str0, Str) --> qtext(Str0, Str), !.

quoted_string2_el(Str0, Str) --> quoted_pair(Str0, Str).

qtext(Str0, Str) -->

   qtext_char(Str0, Str1),
   qtext_chars(Str1, Str).

qtext_char(Str0, Str) -->

   linear_white_space(Str0, Str), !.

qtext_char(_, _) -->

   ( "\"" %"
   ; "\\" %"
   ; "\r" ), !,
   { fail }.

qtext_char([C|Str], Str) -->

   [C].

qtext_chars(Str0, Str) -->

   qtext_char(Str0, Str1), !,
   qtext_chars(Str1, Str).

qtext_chars(Str, Str) --> [].

/*
     domain-literal =  "[" *(dtext / quoted-pair) "]"


     dtext       =  <any CHAR excluding "[",     ; => may be folded
                     "]", "\" & CR, & including
                     linear-white-space>
*/
/*
     comment     =  "(" *(ctext / quoted-pair / comment) ")"

     ctext       =  <any CHAR excluding "(",     ; => may be folded
                     ")", "\" & CR, & including
                     linear-white-space>

*/

comment(Str0, Str) -->

   "(", comment2(Str0, Str), ")".

comment2(Str0, Str) -->

   comment2_el(Str0, Str1), !,
   comment2(Str1, Str).

comment2(Str, Str) --> [].

comment2_el(Str0, Str) --> ctext(Str0, Str), !.

comment2_el(Str0, Str) --> quoted_pair(Str0, Str), !.

comment2_el(Str0, Str) --> comment(Str0, Str).

ctext(Str0, Str) -->

   ctext_char(Str0, Str1),
   ctext_chars(Str1, Str).

ctext_char(Str0, Str) -->

   linear_white_space(Str0, Str), !.

ctext_char(_, _) -->

   ( "(" ; ")" ; "\\" ; "\r" ), !, { fail }.

ctext_char([C|Str], Str) -->

   [C].

ctext_chars(Str0, Str) -->

   ctext_char(Str0, Str1), !,
   ctext_chars(Str1, Str).

ctext_chars(Str, Str) --> [].

/*
     quoted-pair =  "\" CHAR                     ; may quote any char
*/

quoted_pair([C|Str], Str) -->

   "\\", char(C).


/*
     phrase      =  1*word                       ; Sequence of words

     word        =  atom / quoted-string

*/


/*
    6.1.  SYNTAX

     address     =  mailbox                      ; one addressee
                 /  group                        ; named list

     group       =  phrase ":" [#mailbox] ";"

     mailbox     =  addr-spec                    ; simple address
                 /  phrase route-addr            ; name & addr-spec

     route-addr  =  "<" [route] addr-spec ">"

     route       =  1#("@" domain) ":"           ; path-relative

     addr-spec   =  local-part "@" domain        ; global address

     local-part  =  word *("." word)             ; uninterpreted
                                                 ; case-preserved

     domain      =  sub-domain *("." sub-domain)

     sub-domain  =  domain-ref / domain-literal

     domain-ref  =  atom                         ; symbolic reference
*/

%email_address(Email) --> mailbox(Email). %TODO group

%mailbox(Email) --> addr_spec(Email). %TODO phrase route-addr

%addr_spec(Email) -->

%   local_part(Local), "@", domain(Domain),
%   { concat_atom([Local, Domain], '@', Email) }.

%local_part(Local) -->

%   word(Word),
%   dotted_words(Words),
%   { concat_atom([Word|Words], '.', Local) }.

%word(Word) --> atom(Word). %TODO quoted_string












