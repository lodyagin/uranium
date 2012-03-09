% -*- fill-column: 65; -*-

:- begin_tests(ur_url).
:- use_module(u(ur_url)).

test(url_normalize1,
     [Norm_Url == 'http://localhost/eshops']
    ) :-

   url_normalize('http://localhost/eshops',
                 Norm_Url).

test(url_normalize2,
     [Norm_Url == 'http://localhost/eshops']
    ) :-

   url_normalize('http://localhost/eshops/',
                 Norm_Url).

test(url_normalize3,
     [Norm_Url == 'http://localhost/eshops']
    ) :-

   url_normalize('http://localhost/eshops//',
                 Norm_Url).

:- end_tests(ur_url).
