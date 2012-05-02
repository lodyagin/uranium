:- module(common_internet_data,
          [default_port/2
          ]).

/** <module> Common parameters of Internet.
*/

% scheme to default port mapping

default_port(http, 80) :- !.
default_port(https, 443) :- !.
default_port(ws, 80) :- !.
default_port(wss, 443) :- !.

