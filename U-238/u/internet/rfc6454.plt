:- begin_tests(rfc6454).
:- use_module(rfc6454).

% Examples from 3.2.1 chapter of RFC

test(ch3_2_1_1) :-

   uri_origin('http://example.com/', O),
   uri_origin('http://example.com:80/', O),
   uri_origin('http://example.com/path/file', O),
   assertion(O == origin(http, 'example.com', 80)),
   origin_ascii(O, 'http://example.com').

test(ch3_2_1_2) :-

   uri_origin('http://example.com/', O1),
   uri_origin('http://example.com:8080/', O2),
   uri_origin('http://www.example.com/', O3),
   uri_origin('https://example.com:80/', O4),
   uri_origin('https://example.com/', O5),
   uri_origin('http://example.org/', O6),
   uri_origin('http://ietf.org/', O7),

   origin_ascii(O1, A1),
   origin_ascii(O2, A2),
   origin_ascii(O3, A3),
   origin_ascii(O4, A4),
   origin_ascii(O5, A5),
   origin_ascii(O6, A6),
   origin_ascii(O7, A7),

   assertion(A1 == 'http://example.com'),
   assertion(A2 == 'http://example.com:8080'),
   assertion(A3 == 'http://www.example.com'),
   assertion(A4 == 'https://example.com:80'),
   assertion(A5 == 'https://example.com'),
   assertion(A6 == 'http://example.org'),
   assertion(A7 == 'http://ietf.org').

test(login_path) :-

   uri_origin('http://serg:1@example.com/', O),
   uri_origin('http://serg@example.com:80/', O),
   uri_origin('http://example.com/path/file', O),
   assertion(O == origin(http, 'example.com', 80)).


:- end_tests(rfc6454).
