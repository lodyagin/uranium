%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(html_tag_form_v, []).

/** <module> Html form tag.

  ==
  <!ELEMENT FORM - - (%block;|SCRIPT)+ -(FORM) -- interactive form -->
<!ATTLIST FORM
  %attrs;                          -- %coreattrs, %i18n, %events --
  action  %URI;          #REQUIRED -- server-side form handler --
  method  (GET|POST)     GET       -- HTTP method used to submit the form--
  enctype %ContentType;  "application/x-www-form-urlencoded"
  accept  %ContentTypes; #IMPLIED  -- list of MIME types for file upload --
  name        CDATA          #IMPLIED  -- name of form for scripting --
  onsubmit    %Script;       #IMPLIED  -- the form was submitted --
  onreset     %Script;       #IMPLIED  -- the form was reset --
  accept-charset %Charsets;  #IMPLIED  -- list of supported charsets --
  >
  ==
*/

:- use_module(u(v)).
:- use_module(u(ixpath)).
:- use_module(html_tag_v).

new_class(html_tag_form_v, html_tag_v,
          [
           '.action',
           '.method',
           '.enctype',
           '.accept',
           '.name',
           '.onsubmit',
           '.onreset',
           '.accept-charset'
           ]).

% html_tag_form_v will be downcast to form_<name>_v which
% is descendant of it.
new_class(form_v, html_tag_form_v, []).

% Represent all input fields as an object fields
'html_tag_form_v?'(Object, class, Class) :-

   Ctx = context('html_tag_form_v?'/3, _),

   obj_unify(Object,
             ['.id', '.class'], [Tag_Id, Tag_Class]),

   (  nonvar(Tag_Id)
   -> Name = Tag_Id
   ;  nonvar(Tag_Class)
   -> Name = Tag_Class
   ;  functor(Object, Class, _)
   ),

   ( nonvar(Class) -> true  % FIXME
   ;
     concat_atom([form, Name, v], '_', Class),
     form_class_fields(Object, Fields),
     (  class_name(Class)
     -> class_fields(Class, All_Fields),
        (  ord_subset(Fields, All_Fields)
        -> true
        ;  throw(error(class_exists(Class), Ctx))
        )
     ;
        class_create(Class, form_v, Fields)
     )
   ).

downcast(html_tag_form_v, Class_To, From, To) :-

   atom_concat(form_, _, Class_To),
   gen_html_tag_downcast(From, To),
   fill_default_values(From, To).

form_class_fields(Obj, Fields) :-

      findall(Field_Name,
              ixpath(//input(@name=Field_Name), Obj, _),
              Fields1),
      sort(Fields1, Fields2),
      maplist(atom_concat('..'), Fields2, Fields).

fill_default_values(From, To) :-

   findall(v(Field, Default_Value),
           (  ixpath(//input(@value=Default_Value,
                             @name=Name),
                     From, _),
              atom_concat('..', Name, Field)
	   ),
           Fields),

   foreach(
           member(v(Field, Value), Fields),
           ignore(obj_field(To, Field, Value))
          ),

   % radio buttons

   findall(v(Name1, Default_Value),
           (  bagof(radio(Value, Checked),
                    Radio^(ixpath(//input(@type=radio, @value=Value,
                                          @name=Name),
                                  [v], From, Radio),
                           obj_field(Radio, '.checked', Checked)
                          ),
                    Radios
              ),

              (  member(radio(Value, Checked), Radios),
                 nonvar(Checked),
                 (  downcase_atom(Checked, checked)
                 -> true
                 ;  print_message(warning,
                                  html_invalid_attribute_value(checked,
                                                               Checked)),
                    fail
                 )
              ->
                 Default_Value = Value
              ;
                 Radios = [radio(Default_Value, _) | _]
              ),
              atom_concat('..', Name, Name1)
           ),
           Radio_Fields
          ),

   foreach(
           member(v(Field, Value), Radio_Fields),
           ignore(obj_field(To, Field, Value))
          ).

   % TODO checkboxes

   % writeln(Radio_Fields).



