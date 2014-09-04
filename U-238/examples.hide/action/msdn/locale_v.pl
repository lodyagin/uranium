% This file is a part of Uranium, a general-purpose functional test platform.
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
% 
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% -------------------------------------------------------------------------------
%

:- module(locale_v, []).

new_class(locale_object_v, object_v, [lcid, os], [lcid, os]).

new_class(locale_v, locale_object_v,
          [culture_name,
           language_country,
           language_english_name,
           language_native_name,
           ansi_codepage, oem_codepage,
           country_or_region_name_abbreviation,
           language_name_abbreviation,
           number_formatting,
           currency_formatting,
           time_formatting,
           date_formatting,
           calendar,

           language_strings,
           country_strings
          ]
         ).

new_class(number_formatting_v, locale_object_v,
          [positive_number_example,
           negative_number_example,
           decimal_separator,
           digits_after_decimal_separator,
           digit_grouping,
           thousands_separator,
           negative_format_symbol,
           number_list_separator,
           'leading_and_trailing_zeros_0.700',
           standard_digits,
           system_of_measurements]
         ).

new_class(currency_formatting_v, locale_object_v,
          [positive_sample,
           negative_sample,
           currency_symbol,
           decimal_separator,
           thousand_separator,
           digit_grouping]
         ).

new_class(time_formatting_v, locale_object_v,
          [time_sample,
           default_time_format,
           time_format_separator,
           am_symbol,
           pm_symbol,
           all_time_formats]
         ).

new_class(date_formatting_v, locale_object_v,
          [default_short_date_format,
           default_long_date_format,
           all_short_date_formats,
           all_long_date_formats]
         ).

new_class(calendar_v, locale_object_v, [calendars]).