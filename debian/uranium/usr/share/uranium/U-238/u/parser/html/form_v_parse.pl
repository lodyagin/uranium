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

:- use_module(u(ur_lists)).
:- use_module(u(v)).
:- use_module(u(ixpath)).

form_v_parse(Object, Object).

%   findall(Input_Field,
%           parse_form_input_field(Object0, Input_Field),
%           Input_Field_List),
%   obj_downcast(Object0, form_v, Object),
%   obj_field(Object, input_field_list, Input_Field_List).


% % Return all input fields on bt

% parse_form_input_field(Form_Obj, Input_Field_Object) :-

%   ixpath(//input, [vix], Form_Obj, Input_Obj),
%   % element(input, Attr_List, _)
%   once(  select(value = Value_Attr, Attr_List, AL2)
%   -> select(default_value = Value_Attr, AL3, AL2)
%   ;  AL3 = Attr_List
%   ),
  
%   (  selectchk(class = _, AL3, AL4) -> true
%   ;  AL4 = AL3
%   ),
        
%   corteging(=, Field_Names, Field_Values, AL4),
%   obj_construct_weak(form_input_field_v, Field_Names, Field_Values,
% 		     Object1),
%   obj_downcast(Object1, Input_Field_Object).

