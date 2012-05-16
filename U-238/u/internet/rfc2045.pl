:- module(rfc2045,
          [media_type//3
          ]).

:- use_module(u(internet/rfc822)).

/*
5.1.  Syntax of the Content-Type Header Field

   In the Augmented BNF notation of RFC 822, a Content-Type header field
   value is defined as follows:

     content := "Content-Type" ":" type "/" subtype
                *(";" parameter)
                ; Matching of media type and subtype
                ; is ALWAYS case-insensitive.

     type := discrete-type / composite-type

     discrete-type := "text" / "image" / "audio" / "video" /
                      "application" / extension-token

     composite-type := "message" / "multipart" / extension-token

     extension-token := ietf-token / x-token

     ietf-token := <An extension token defined by a
                    standards-track RFC and registered
                    with IANA.>

     x-token := <The two characters "X-" or "x-" followed, with
                 no intervening white space, by any token>

     subtype := extension-token / iana-token

     iana-token := <A publicly-defined extension token. Tokens
                    of this form must be registered with IANA
                    as specified in RFC 2048.>

     parameter := attribute "=" value

     attribute := token
                  ; Matching of attributes
                  ; is ALWAYS case-insensitive.

     value := token / quoted-string

     token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                 or tspecials>

     tspecials :=  "(" / ")" / "<" / ">" / "@" /
                   "," / ";" / ":" / "\" / <">
                   "/" / "[" / "]" / "?" / "="
                   ; Must be in quoted-string,
                   ; to use within parameter values

  */

media_type(Type, Subtype, Pars) -->

   type(Type), "/", subtype(Subtype), parameters(Pars).

type(Type) --> discrete_type(Type) ; composite_type(Type).

discrete_type(text)  --> ci("text"), !.
discrete_type(image) --> ci("image"), !.
discrete_type(audio) --> ci("audio"), !.
discrete_type(video) --> ci("video"), !.
discrete_type(application) --> ci("application"), !.
discrete_type(Type) --> extension_token(Type).

composite_type(message) --> ci("message"), !.
composite_type(multipart) --> ci("multipart"), !.
composite_type(Type) --> extension_token(Type).

extension_token(Type) --> ietf_token(Type) ; x_token(Type).

% TODO: get it
ietf_token(_) --> { fail }.

x_token(Type) -->
   { Str0 = "x-" },
   ci(Str0), token(Str1),
   { append(Str0, Str1, Str),
     atom_codes(Type1, Str),
     downcase_atom(Type1, Type)
   }.

subtype(Subtype) --> extension_token(Subtype) ; iana_token(Subtype).

% TODO: add more from http://www.iana.org/assignments/media-types/index.html
iana_token(html) --> "html".

parameters([Par|Pars]) -->
   linear_white_space_if_present(_, _, _, _),
   parameter(Par), !,
   parameters(Pars).

parameters([]) -->
   [].

parameter(Attribute = Value) -->
   attribute(Attribute), "=", value(Value).

attribute(Attr) -->
   token(Str),
   { atom_codes(Str, Attr1),
     downcase_atom(Attr1, Attr) }.

token([C|Str0]) -->
   token_el(C),
   token_els(Str0).

token_el(_) --> " ", { fail }, !.
token_el(_) --> ctl(_), { fail }, !.
token_el(_) --> tspecials(_), { fail }, !.
token_el(C) --> [C], { between(0, 127, C) }.

token_els([C|Str1]) -->
   token_el(C), !,
   token_els(Str1).

token_els([]) -->
   [].

tspecials(C) -->
   [C],
   { member(C, "()<>@,;:\\\"/[]?=") }.

ci([C|Str1]) --> cic(C), !, ci(Str1).
ci([]) --> [].

cic(L) --> [C], { code_type(L, to_lower(C)) }.


   