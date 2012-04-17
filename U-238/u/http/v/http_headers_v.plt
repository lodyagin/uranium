:- begin_tests(http_headers_v).
:- use_module(http_headers_v).

test(list_to_http_headers_v) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              transfer_encoding = chunked],
   Headers_Ref = ['Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Transfer-Encoding' = chunked],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_general_headers_v, http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   assertion(Headers2 == Headers_Ref).

test(list_to_http_headers_v_functor_form) :-

   Headers = [date('Tue, 03 Apr 2012 19:32:50 GMT'),
              transfer_encoding(chunked)],
   Headers_Ref = ['Date' ='Tue, 03 Apr 2012 19:32:50 GMT',
                  'Transfer-Encoding' = chunked],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_general_headers_v, http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   assertion(Headers2 == Headers_Ref).

test(list_to_http_request_headers_v1) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              accept = 'text/html',
              host = 'kogorta.dp.ua',
              transfer_encoding = chunked
              ],
   Headers_Ref = ['Date' ='Tue, 03 Apr 2012 19:32:50 GMT',
                  'Accept' = 'text/html',
                  'Host' = 'kogorta.dp.ua',
                  'Transfer-Encoding' = chunked
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_request_headers_v,
             http_general_headers_v,
             http_headers_v, 
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1).

test(list_to_http_request_headers_v2) :-

   Headers = [accept = 'text/html',
              date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              host = 'kogorta.dp.ua',
              transfer_encoding = chunked
              ],
   Headers_Ref = ['Accept' = 'text/html',
                  'Date' ='Tue, 03 Apr 2012 19:32:50 GMT',
                  'Host' = 'kogorta.dp.ua',
                  'Transfer-Encoding' = chunked
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_request_headers_v,
             http_general_headers_v,
             http_headers_v, 
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1).

test(list_to_http_response_headers_v) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              location = 'http://kogorta.dp.ua/'
              ],
   Headers_Ref = ['Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Location' = 'http://kogorta.dp.ua/'
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_response_headers_v,
             http_general_headers_v,
             http_headers_v, 
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1).

test(list_to_http_invalid_mixed_headers_v) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              host = 'kogorta.dp.ua',
              location = 'http://kogorta.dp.ua/',
              transfer_encoding = chunked
              ],
   Headers_Ref = ['Date' ='Tue, 03 Apr 2012 19:32:50 GMT',
                  'Host' = 'kogorta.dp.ua',
                  'Location' = 'http://kogorta.dp.ua/',
                  'Transfer-Encoding' = chunked
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_mixed_headers_v,
             http_invalid_headers_v,
             http_response_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1).

test(list_to_http_invalid_bulk_headers_v1) :-

   Headers = [bulk1 = 1, '' = 3],
   Headers_Ref = ['Bulk1' = 1, '' = 3],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk == Headers).

test(list_to_http_invalid_bulk_headers_v2) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              bulk1 = 1,
              transfer_encoding = chunked],
   Headers_Ref = ['Date' ='Tue, 03 Apr 2012 19:32:50 GMT',
                  'Bulk1' = 1,
                  'Transfer-Encoding' = chunked],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk == [bulk1 = 1]).

test(list_to_http_invalid_bulk_headers_v3) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              accept = 'text/html',
              host = 'kogorta.dp.ua',
              transfer_encoding = chunked,
              bulk1 = 1
              ],
   Headers_Ref = ['Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Accept' = 'text/html',
                  'Host' = 'kogorta.dp.ua',
                  'Transfer-Encoding' = chunked,
                  'Bulk1' = 1
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk == [bulk1 = 1]).

test(list_to_http_invalid_bulk_headers_v4) :-

   Headers = [bulk1 = 1,
              date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              accept = 'text/html',
              host = 'kogorta.dp.ua',
              transfer_encoding = chunked
              ],
   Headers_Ref = ['Bulk1' = 1,
                  'Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Accept' = 'text/html',
                  'Host' = 'kogorta.dp.ua',
                  'Transfer-Encoding' = chunked
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk == [bulk1 = 1]).

test(list_to_http_invalid_bulk_headers_v5) :-

   Headers = [bulk1 = 1,
              date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              bulk2 = 2,
              host = 'kogorta.dp.ua',
              bulk3 = 3,
              location = 'http://kogorta.dp.ua/',
              bulk4 = 4,
              transfer_encoding = chunked,
              bulk5 = 5
              ],
   Headers_Ref = ['Bulk1' = 1,
                  'Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Bulk2' = 2,
                  'Host' = 'kogorta.dp.ua',
                  'Bulk3' = 3,
                  'Location' = 'http://kogorta.dp.ua/',
                  'Bulk4' = 4,
                  'Transfer-Encoding' = chunked,
                  'Bulk5' = 5
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_mixed_headers_v,
             http_invalid_headers_v,
             http_response_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk ==
            [bulk1 = 1, bulk2 = 2, bulk3 = 3,
             bulk4 = 4, bulk5 = 5
            ]).

test(list_to_http_invalid_bulk_headers_v6) :-

   Headers = [date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              bulk1 = 1,
              bulk2 = 2,
              host = 'kogorta.dp.ua',
              bulk3 = 3,
              location = 'http://kogorta.dp.ua/',
              bulk4 = 4,
              transfer_encoding = chunked,
              bulk5 = 5
              ],
   Headers_Ref = ['Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Bulk1' = 1,
                  'Bulk2' = 2,
                  'Host' = 'kogorta.dp.ua',
                  'Bulk3' = 3,
                  'Location' = 'http://kogorta.dp.ua/',
                  'Bulk4' = 4,
                  'Transfer-Encoding' = chunked,
                  'Bulk5' = 5
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_mixed_headers_v,
             http_invalid_headers_v,
             http_response_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk ==
            [bulk1 = 1, bulk2 = 2, bulk3 = 3,
             bulk4 = 4, bulk5 = 5
            ]).

test(list_to_http_invalid_bulk_headers_v7) :-

   Headers = [
              date = 'Tue, 03 Apr 2012 19:32:50 GMT',
              host = 'kogorta.dp.ua',
              location = 'http://kogorta.dp.ua/',
              bulk1 = 1,
              bulk2 = 2,
              bulk3 = 3,
              bulk4 = 4,
              transfer_encoding = chunked,
              bulk5 = 5
              ],
   Headers_Ref = [
                  'Date' = 'Tue, 03 Apr 2012 19:32:50 GMT',
                  'Host' = 'kogorta.dp.ua',
                  'Location' = 'http://kogorta.dp.ua/',
                  'Bulk1' = 1,
                  'Bulk2' = 2,
                  'Bulk3' = 3,
                  'Bulk4' = 4,
                  'Transfer-Encoding' = chunked,
                  'Bulk5' = 5
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [
             http_invalid_bulk_headers_v,
             http_invalid_mixed_headers_v,
             http_invalid_headers_v,
             http_response_headers_v,
             http_request_headers_v,
             http_general_headers_v,
             http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers20, Obj),
   msort(Headers20, Headers2),
   sort(Headers_Ref, Headers1),
   assertion(Headers2 == Headers1),
   obj_field(Obj, '@bulk', Bulk_Rev),
   reverse(Bulk_Rev, Bulk),
   assertion(Bulk ==
            [bulk1 = 1, bulk2 = 2, bulk3 = 3,
             bulk4 = 4, bulk5 = 5
            ]).

test(repeated_headers1) :-

   Headers = [accept_language = en,
              accept_language = ru
              ],
   Headers_Ref = ['Accept-Language' = en,
                  'Accept-Language' = ru
                 ],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_request_headers_v, http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   assertion(Headers2 == Headers_Ref).

test(repeated_headers2) :-

   Headers = [accept_language = 'en, ru'],
   Headers_Ref = ['Accept-Language' = 'en, ru'],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_request_headers_v, http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   assertion(Headers2 == Headers_Ref).

test(repeated_headers3) :-

   Headers = [accept_language = [en, ru]],
   Headers_Ref = ['Accept-Language' = [en, ru]],
   http_headers_list_obj(Headers, Obj),
   obj_parents(Obj, Parents),
   assertion(Parents ==
            [http_request_headers_v, http_headers_v,
             object_v, object_base_v]),
   http_headers_list_obj(Headers2, Obj),
   assertion(Headers2 == Headers_Ref).

:- end_tests(http_headers_v).
