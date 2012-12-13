%  -*- fill-column: 65; -*-
% 
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2012, Kogorta OOO Ltd.
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

%  This contains printing rules for all messages in Uranium.

:- module(ur_messages, [prolog:message//1]).

:- use_module(u(v)).

:- multifile prolog:message//1.

% Error messages for the Uranium object system

prolog:message(class_system_bad_state(Details)) -->

   ['The Uranium class system may be corrupted: '],
   [ nl ],
   [Details].

prolog:message(implementation_error(Format, Args)) -->

   ['Internal Uranium error: ', nl],
   [Format - Args].

prolog:message(class_exists(Class)) -->

   ['The class ~a is defined already' - [Class]].

prolog:message(class_inheritance_cycle(Graph)) -->

   ['There is a cycle in class inheritance: ~w'
    - Graph].

prolog:message(type_redefined(Type, Orig_Class)) -->

   ['Type ~a is already defined in class ~a'
    - [Type, Orig_Class]],
   [nl, 'The new definition was ignored.'].

prolog:message(invalid_object(Object, Details)) -->

   ['Invalid object passed: ~p ' - [Object]],
   [ '(', Details, ')' ].

prolog:message(no_object_field(Object, Field_Name)) -->

   ['There is no such field `~a'' in the object ~p'
    - [Field_Name, Object]].

prolog:message(undef_operation(Op_Name, Class_Id)) -->

   ['The operation `~a'' is not defined for class id ~d'
   - [Op_Name, Class_Id]].

prolog:message(bad_eval_result(Object, Field)) -->

   ['User-defined field `~a'' evaluation failed for ~p'
   - [Field, Object]].

prolog:message(bad_downcast_impl(Mode, Object, Class_From,
                                 Class_To, Result)) -->

   ['User-defined ~a implementation is bad:' - [Mode]],
   [ nl ],
   ['~a -> ~a transforms ~p to ~p'
   - [Class_From, Class_To, Object, Result]].

prolog:message(bad_downcast_impl(Term, Message)) -->

   ['User-defined ~p is bad: ~a' - [Term, Message]].

prolog:message(not_downcast(From_Class, To_Class)) -->

   ['~a -> ~a is not downcast' - [From_Class, To_Class]].

prolog:message(old_base_is_invalid(Old_Base, Orig_Id)) -->

   ['~a is not a base for the class with id ~d'
   - [Old_Base, Orig_Id]].

prolog:message(cant_rebase_to_object_base_v) -->

   ['Can\'t rebase to object_base_v'].

prolog:message(insufficient_class_order(Order, Orig_Order)) -->

   ['The defined class order ~p is insufficient ' - [Order]],
   ['for ordering a class with the parents ~p' - [Orig_Order]].


prolog:message(ignore_cookie_attribute(Attr, Val, Obj)) -->

   { obj_field(Obj, set_cookie, Set_Cookie) },
   ['Ignore [~a=~a] for ~p' - [Attr, Val, Set_Cookie]].


prolog:message(class_definition_error(Module, Term)) -->

   ['Invalid term in the class definition module ~a: ~p'
   - [Module, Term]].

% ixpath
prolog:message(ixpath_not_implemented(What, Expr)) -->

   ['ixpath: ~a is not implemented (the expression is ~w)'
   - [What, Expr]].

% html parsing

prolog:message(html_invalid_attribute_value(Attr, Val)) -->

   ['invalud value ~p for html attribute ~a' - [Val, Attr]].

% Uranium DB

prolog:message(db_system_bad_state(Format, Args)) -->

   ['The Uranium DB system may be corrupted: '],
   [ nl ],
   [Format - Args].

prolog:message(bad_db(DB_Key, Format, Args)) -->

   ['Bad DB ~w~n' - [DB_Key]],
   [Format - Args].

prolog:message(db_key_exists(DB_Key, DB_Object, New_Object)) -->

   ['Duplicate DB key for DB ~a when put ~p'
   - [DB_Key, New_Object]],
   [ nl ],
   [ 'The existing object in DB: ~p' - [DB_Object]].

prolog:message(class_fields_mismatch(DB_Key, Class, Local, DB))
-->

   ['The DB ~a has other fields for the class ~a~n'
    - [DB_Key, Class]],
   ['our fields: ~w ~ndb  fields: ~w' - [Local, DB]].

prolog:message(class_parents_mismatch(DB_Key, Class, Local, DB))
-->

   ['The DB ~a has other parents for the class ~a~n'
    - [DB_Key, Class]],
   ['our parents: ~w ~ndb  parents: ~w' - [Local, DB]].

prolog:message(invalid_db_class_id(DB_Key, DB_Class_Id)) -->

   ['The db class id ~w is not found in db ~w'
    - [DB_Class_Id, DB_Key]].

prolog:message(db_obj_replace_protector(DB_Key,
               Protector, Object)) -->

   ['Replace protector ~a when try add ~p into db ~a~n'
   - [Protector, Object, DB_Key]].

prolog:message(db_unformatted_object(DB_Key, Parents)) -->

   ['Try to put unformatted object in DB ~a (parents: ~p)'
   - [DB_Key, Parents]].

prolog:message(db_invalid_query(DB_Key, Query)) -->

   ['Invalid query ~w on DB ~a' - [Query, DB_Key]].

%prolog:message(db_ungrounded_key(Key, Key_Value, Action)) -->

%   ['When try ~a the ungrounded key ~p = ~p was found'
%   - [Action, Key, Key_Value]].

% randgen

prolog:message(unknown_random_generator(Family, Name)) -->

   ['The random generator ~a/~a is unknown' - [Family, Name]].

% ur_option

prolog:message(invalid_option_definition(Rule)) -->

   ['The option definition ~p is invalid' - [Rule]].
