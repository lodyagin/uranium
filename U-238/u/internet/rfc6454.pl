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

:- module(rfc6454,
          [uri_origin/2,
           origin_ascii/2
          ]).

/** <module> RFC 6454 - The Web Origin Concept
*/

:- use_module(library(uri)).
:- use_module(u(internet/common_internet_data)).

uri_origin(Uri, Origin) :-

   /*
   4.  Origin of a URI

   The origin of a URI is the value computed by the following algorithm:

   N/I
   1.  If the URI does not use a hierarchical element as a naming
       authority (see [RFC3986], Section 3.2) or if the URI is not an
       absolute URI, then generate a fresh globally unique identifier
       and return that value. ...

   2.  Let uri-scheme be the scheme component of the URI, converted to
       lowercase.
   */

   uri_components(Uri, Parts),
   uri_data(scheme, Parts, Scheme1),
   uri_data(authority, Parts, Authority1),
   uri_authority_components(Authority1, Authority_Parts),
   uri_authority_data(host, Authority_Parts, Host1),
   downcase_atom(Scheme1, Scheme),

/*
   3.  If the implementation doesn't support the protocol given by uri-
       scheme, then generate a fresh globally unique identifier and
       return that value.

   4.  If uri-scheme is "file", the implementation MAY return an
       implementation-defined value. ...

   *FIXME: non-ASCII conversion, check collation with rfc4790*
   5.  Let uri-host be the host component of the URI, converted to lower
       case (using the i;ascii-casemap collation defined in [RFC4790]).

          NOTE: This document assumes that the user agent performs
          Internationalizing Domain Names in Applications (IDNA)
          processing and validation when constructing the URI.  In
          particular, this document assumes the uri-host will contain
          only LDH labels because the user agent will have already
          converted any non-ASCII labels to their corresponding A-labels
          (see [RFC5890]).  For this reason, origin-based security
          policies are sensitive to the IDNA algorithm employed by the
          user agent.  See Section 8.4 for further discussion.
*/
   downcase_atom(Host1, Host),

/*
   6.  If there is no port component of the URI:

       1.  Let uri-port be the default port for the protocol given by
           uri-scheme.

       Otherwise:

       2.  Let uri-port be the port component of the URI.
*/
   uri_authority_data(port, Authority_Parts, Port),
   (  var(Port)
   -> default_port(Scheme, Port) % <NB> can fail if unknown Scheme
   ;  uri_authority_data(port, Authority_Parts, Port)
   ),
/*
   7.  Return the triple (uri-scheme, uri-host, uri-port).
*/
  Origin = origin(Scheme, Host, Port).

origin_ascii(origin(Scheme, Host, Port), Result) :-

   nonvar(Scheme), nonvar(Host), nonvar(Port), !,

   /*
6.2.  ASCII Serialization of an Origin

   1.  <see below>

   2.  Otherwise, let result be the scheme part of the origin triple.

   3.  Append the string "://" to result.

   4.  Append the host part of the origin triple to result.

   5.  If the port part of the origin triple is different from the
       default port for the protocol given by the scheme part of the
       origin triple:

       1.  Append a U+003A COLON code point (":") and the given port, in
           base ten, to result.

   6.  Return result.
   */

   concat_atom([Scheme, Host], '://', Part1),
   (  default_port(Scheme, Port)
   -> Result = Part1
   ;  format(atom(Result), '~a:~d', [Part1, Port])
   ).

origin_ascii(_, null).
/*
   1.  If the origin is not a scheme/host/port triple, then return the
       string

          null
*/
