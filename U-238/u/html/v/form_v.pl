% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(form_v, []).

:- use_module(u(v)).

'form_input_field_v?'(Object, class, Value) :-

  obj_field(Object, type, Type),
  concat_atom(['form_', Type, '_field_v'], Value).

new_class(form_v, http_result_v,
          [tag_id, tag_class,
           input_field_list], []).

new_class(form_input_field_v, object_v,
          [type, name, default_value, current_value]).

new_class(form_button_field_v, form_input_field_v, []).

new_class(form_checkbox_field_v, form_input_field_v, []).

new_class(form_file_field_v, form_input_field_v, []).

new_class(form_hidden_field_v, form_input_field_v, []).

new_class(form_image_field_v, form_input_field_v, []).

new_class(form_password_field_v, form_text_field_v, []).

new_class(form_radio_field_v, form_input_field_v, [size]).

new_class(form_reset_field_v, form_input_field_v, [size]).

new_class(form_submit_field_v, form_input_field_v, []).

new_class(form_text_field_v, form_input_field_v, [size]).




