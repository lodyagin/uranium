:- begin_tests(cookie_v).
:- use_module(cookie_v).

% rfc 6265, 5.1.3

test(domain_match1) :-
   domain_match('ABC.d.E', 'abc.D.E').

test(domain_match2) :-
   domain_match('d.E', 'abc.D.E').

test(domain_match3, [fail]) :-
   domain_match('.d.E', 'abc.D.E').

% rfc 6265, 5.1.4, 1-4

test(uri_default_path1, [P = '/']) :-
   cookie_v:uri_default_path(_, P).

test(uri_default_path2, [P = '/']) :-
   cookie_v:uri_default_path('', P).

test(uri_default_path3, [P = '/']) :-
   cookie_v:uri_default_path('a/', P).

test(uri_default_path4, [P = '/']) :-
   cookie_v:uri_default_path('/', P).

test(uri_default_path5, [P = '/a/b']) :-
   cookie_v:uri_default_path('/a/b', P).

test(uri_default_path6, [P = '/a/b']) :-
   cookie_v:uri_default_path('/a/b/', P).

% rfc 6265, 5.2.3

test(normalize_domain1, [D = 'a.b.c']) :-
   cookie_v:normalize_domain('.a.b.c', D).

test(normalize_domain2, [D = 'a.b.c']) :-
   cookie_v:normalize_domain('a.b.c', D).

test(normalize_domain3, [D = 'a']) :-
   cookie_v:normalize_domain('.a', D).

test(normalize_domain4, [D = 'a']) :-
   cookie_v:normalize_domain('a', D).

