%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(locales, [query_locale_info/0]).

:- use_module(state/general/html_user_v).
:- use_module(u(action/find)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(logging)).

query_locale_info :-

   db_clear(db_locale_v0),
   db_clear(db_locale_v1),
   db_clear(db_locale_v2),
   db_clear(db_locale_v3),
   db_clear(db_language_v),
   db_clear(db_country_v),

   new_html_user(U1),
   supported_lang_list(Langs),
   (
    member(Lang, Langs),
    write_log(['basic query', Lang], [lf(1)]),
    find(msdn, U1, locale, [language_english_name(Lang)], db_locale_v0),
    db_size(db_locale_v0, V0_Size),
    write_log(['found', V0_Size], [lf(1)]),
    db_copy(db_locale_v0, db_locale_v1),
    fail
   ;
    true
   ),
   dump_db(db_locale_v1),
   db_size(db_locale_v1, Locale_V1_Size),
   write_log(['locale V1 size', Locale_V1_Size], [lf(1)]),
   (  db_iterate(db_locale_v1, true, Locale_V1_Pred),
      obj_field(Locale_V1_Pred, culture_name, V1_CN),
      writeln(V1_CN),
      fail
   ;
      true
   ),

   new_html_user(U2),
   find(msdn, U2, language, [], db_language_v),
   dump_db(db_language_v),

   (  db_iterate(db_language_v, true, L),
      named_args_unify(L,
                       [language_english_name,
                        language_country,
                        language_strings],
                       [A_LEN, A_LC, _]),

      db_iterate_replace(db_locale_v1,
                         add_language(L),
                         language_english_name(A_LEN)
                        /\ (language_country(A_LC)
                           \/ language_country(default)),
                         true,
                         1
                        ),
      fail
   ;
      true
   ),
   write_log('after language join:', [logger(dump_db), lf(1, before)]),
   dump_db(db_locale_v1),
   db_size(db_locale_v1, Locale_V2_Size),
   write_log(['locale V2 size', Locale_V2_Size], [lf(1)]),
   (  db_iterate(db_locale_v1, true, Locale_V2_Pred),
      obj_field(Locale_V2_Pred, culture_name, V2_CN),
      writeln(V2_CN),
      fail
   ;
      true
   ),

   % add country info
   new_html_user(U3),
   find(msdn, U3, country, [], db_country_v),
   dump_db(db_country_v),

   (  db_iterate(db_country_v, true, L2),
      named_args_unify(L2,
                       [country, country_strings],
                       [B_C, B_CS]),

      db_iterate_replace(db_locale_v1,
                         add_country(L2),
                         language_country(B_C),
                         not_added_country(B_CS)
                        ),
      fail
   ;
      true
   ),

   write_log('after country join:', [logger(dump_db), lf(1, before)]),
   dump_db(db_locale_v1),
   db_size(db_locale_v1, Locale_V3_Size),
   write_log(['locale V3 size', Locale_V3_Size], [lf(1)]),

   db_to_list(db_locale_v1, _, Found),
   maplist(obj_pretty_print, Found).


not_added_country(Country_Strings, Locale) :-

  obj_field(Locale, country_strings, CS),
  (  var(CS)
  -> true
  ;  list_to_ord_set(Country_Strings, CS2),
     \+ ord_intersect(CS, CS2)
  ).

show_locale_info :-

  db_iterate(db_locale_v1, true, Locale),
  obj_field(Locale, language_strings, Lang_Strings),
  obj_field(Locale, country_strings, Country_Strings),
  obj_field(Locale, ansi_codepage, Ansi_CP),
  obj_field(Locale, oem_codepage, OEM_CP),
  obj_field(Locale, culture_name, Code),

  write_log([Lang_Strings, '_', Country_Strings, '.[', Ansi_CP, ',', OEM_CP,
             '] ---> ', Code],
            [lf(1)]),
  fail
  ;
  true.


add_language(Lang, Locale_In, Locale_Out, true) :-

  obj_field(Locale_In, language_strings, String),
  obj_field(Locale_In, culture_name, CN),
  obj_field(Lang, language_strings, Add_String),
  obj_field(Lang, language_country, Country),
  (   var(String)
  ->  New_String = Add_String,
      Locale_Out = Locale_In
  ;   obj_reset_fields([language_strings], Locale_In, Locale_Out, _),
      list_to_ord_set(String, String_Ord),
      list_to_ord_set(Add_String, Add_String_Ord),
      ord_union(String_Ord, Add_String_Ord, New_String)
  ),
  obj_field(Locale_Out, language_strings, New_String),
  write_log(['map', Country, 'on', CN], [lf(1)]).

add_country(Country, Locale_In, Locale_Out, true) :-

  obj_field(Locale_In, country_strings, String),
  obj_field(Locale_In, culture_name, CN),
  obj_field(Country, country_strings, Add_String),
  obj_field(Country, country, Country_Name),
  (   var(String)
  ->  New_String = Add_String,
      Locale_Out = Locale_In
  ;   obj_reset_fields([country_strings], Locale_In, Locale_Out, _),
      list_to_ord_set(String, String_Ord),
      list_to_ord_set(Add_String, Add_String_Ord),
      ord_union(String_Ord, Add_String_Ord, New_String)
  ),
  obj_field(Locale_Out, country_strings, New_String),
  write_log(['map', Country_Name, 'on', CN], [lf(1)]).


supported_lang_list(['German']) :- !.

supported_lang_list(['Arabic', 'German', 'English', 'Spanish', 'French',
		     'Italian', 'Kazakh', 'Portuguese', 'Russian',
		     'Ukrainian',
		     'Chinese']).


list_to_query([El], _, El) :- !.

list_to_query([H|T], Fun, NQuery) :-

  list_to_query(T, Fun, Query),
  NQuery =.. [Fun, H, Query].
