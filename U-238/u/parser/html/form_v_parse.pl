% -*- fill-column: 65; -*- 
% _____________________________________________________________
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011  Sergei Lodyagin
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
% 
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
% _____________________________________________________________

:- module(form_v_parse,
          [form_v_parse/2   % +Data, -Object
          ]).

:- use_module(library(ur_lists)).
:- use_module(library(ur_objects)).

form_v_parse(DOM, Object) :-

  findall(Input_Field,
          parse_form_input_field(DOM, Input_Field),
          Input_Field_List),
  obj_construct(form_v, [input_field_list], [Input_Field_List],
                Object).

                                
% Return all input fields on bt
  
parse_form_input_field(DOM, Input_Field_Object) :-

  xpath(DOM, //input, element(input, Attr_List, _)),
  once(  select(value = Value_Attr, Attr_List, AL2)
  -> select(default_value = Value_Attr, AL3, AL2)
  ;  AL3 = Attr_List
  ),
  corteging(=, Field_Names, Field_Values, AL3),
  obj_construct(form_input_field_v, Field_Names, Field_Values,
                Object1, weak),
  obj_downcast(Object1, Input_Field_Object).

