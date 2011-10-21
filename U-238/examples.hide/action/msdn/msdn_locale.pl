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

:- module(msdn_locale, []).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(ur_lists)).
:- use_module(u(ur_atoms)).
:- use_module(u(html/http_page)).
:- use_module(u(html/http_ops)).
:- use_module(parser/html/html_page_parse).
:- use_module(parser/html/html_page_find).
:- use_module(parser/html/table_v_parse).
:- use_module(u(logging)).

:- use_module(u(action/templates/find_all_table_rows)).

find_all_locales(DB_Key, User) :-

    obj_field(User, cookie_db_key, Cookies_DB),

    ground(Cookies_DB),

    http_page(http_ops:http_get_html([], Cookies_DB),
              'http://msdn.microsoft.com/en-us/goglobal/bb896001.aspx',
              _
             ),

    http_page(http_ops:http_get_html([], Cookies_DB),
              'http://www.microsoft.com/resources/msdn/goglobal/default.mspx',
              Page
             ),

    find_all_table_rows:find_all_table_rows(DB_Key,
			Page,
			[culture_name, language],
			locale_v,
			cast_expr([piece(lcid_culture_identifier, '0x%'),
				   culture_name,
				   then(piece('localelanguage_country/region',
					      '% ('),
					'localelanguage_country/region'
				   ),
				   then(piece('localelanguage_country/region',
                                              '(%)'),
                                        value(default)
                                       ),
				   %language,
				   local_language_name,
				   ansi_codepage,
				   oem_codepage,
				   'country_or_region_name_abbreviation_*',
				   'language_name_abbreviation_**'],

				  [lcid,
				   culture_name,
				   language_english_name,
				   language_country,
				   language_native_name,
				   ansi_codepage,
				   oem_codepage,
				   country_or_region_name_abbreviation,
				   language_name_abbreviation],

				  [os], ['Windows%20Vista']
			)
			).


fill_full_data_locale(DB_Key, DB_Key2, User, Locale) :-

    obj_field(Locale, lcid, LCID),
    obj_field(Locale, os, OS_Name),

    % define class by key (base_url, id)
    named_arg_unify(DB_Key, Class, lcid, LCID,
                    Locale),
    named_arg_unify(DB_Key, Class, os, OS_Name,
                    Locale),

    (Class = locale_v; class_descendant(locale_v, Class)),

    concat_atom(['http://www.microsoft.com/resources/msdn/goglobal/default.mspx?submitted=', LCID, '&OS=', OS_Name], Url),

    obj_field(User, cookie_db_key, Cookies_DB),
    http_page(http_ops:http_get_html([], Cookies_DB),
              Url,
              Page
             ),

    atom_concat(DB_Key2, '.page', DB_Key3),
    db_clear(DB_Key3),
    html_page_parse(DB_Key3, Page, [table_v]),
    write_log('after html_page_parse:', [logger(dump_db), lf(1)]),
    dump_db(DB_Key3),
    
    atom_concat(DB_Key2, '.table', DB_Key4),
    db_clear(DB_Key4),
    tolower(LCID, LCID_L),
    concat_atom([LCID_L, '_codepage_info'], Table_Name),
    html_page_find(DB_Key3, DB_Key4, table_v, [Table_Name]),
    write_log('after html_page_find:', [logger(dump_db), lf(1)]),
    dump_db(DB_Key4),

    !,

    (
     db_recorded(DB_Key4, Table),
     obj_field(Table, rows, Rows),

     findall(Name - Value,
             member(table_data([Name], [Value]), Rows),
             L2
            ),
     corteging('-', Field_Names1, Field_Values, L2),

     maplist(normalize_row_name, Field_Names1, Field_Names),
     
     obj_construct(number_formatting_v,
                   Field_Names, Field_Values,
                   Number_Formatting, weak),

     obj_construct(currency_formatting_v,
                   Field_Names, Field_Values,
                   Currency_Formatting, weak),

     obj_construct(time_formatting_v,
                   Field_Names, Field_Values,
                   Time_Formatting, weak),

     obj_construct(date_formatting_v,
                   Field_Names, Field_Values,
                   Date_Formatting, weak),

     obj_construct(calendar_v,
                   Field_Names, Field_Values,
                   Calendar, weak),
     
     obj_copy(Locale, Full_Locale),

     named_args_unify(Full_Locale,
                      [number_formatting,
                       currency_formatting,
                       time_formatting,
                       date_formatting,
                       calendar],
                      
                      [Number_Formatting,
                       Currency_Formatting,
                       Time_Formatting,
                       Date_Formatting,
                       Calendar]),

     db_put_object(DB_Key2, Full_Locale),
     fail
    ;
     true
    ),
    
    write_log('after all:', [logger(dump_db), lf(1)]),
    dump_db(DB_Key2).
    



