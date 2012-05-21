%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
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

:- module(ur_options_v,
          []).

:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(vd)).

:- discontiguous new_class/3, new_class/4.

/*
new_class(ur_option_v, object_v, [definition, pattern, default]).

new_class(ur_option_group_v, ur_option_v,
          [group_name, group_type, patterns]).

new_class(ur_option_group_single_v, ur_option_group_v, []).

new_class(ur_option_group_multi_v, ur_option_group_v, []).

new_class(ur_invalid_option_v, ur_option_v, []).

'ur_option_v?'(Obj, class, Class) :-
   obj_field(Obj, definition, Defitinion),
   definition_class(Definition, Class).

definition_class(multi(Group_Name, Patterns, Defaults), ur_option_group_multi_v,
  */

new_class(ur_options_v, object_v,
          [options_in  % as passed by user
          ]
         ).

new_class(gt_strings__random_string_options_v,
          ur_options_v,
          [
           ]).

new_class(option_rule_v, db_object_v,
          [pattern,
           options_object_in,
           'options_object_out#'
           ],
          [pattern]).

new_class(single_option_rule_v, option_rule_v, []).

'ur_options_v?'(Obj0, options_out, Obj) :-

   obj_unify(Obj0, [class, options_in], [Class, Options]),
   process_options(Options, Class, Obj0, Obj).

process_options([], _, Obj, Obj) :- !.
process_options([Option|T], DB, Obj0, Obj) :-
   must_be(compound, Option),
   named_args_unify(DB, _, [pattern], [Option], Rule), !,
   obj_unify(Rule,
             [options_object_in, option_in,
              options_object_out],
             [Obj0, Option,
              Obj1]),
   process_options(T, DB, Obj1, Obj).

'single_option_rule_v?'(Rule, option_in, Option) :-
   functor(Option, Name, _),
   obj_unify(Rule,
             [options_object_in, 'options_object_out#'],
             [Obj, Obj1]),
   obj_rewrite(Obj, [Name], [Old_Value], [Option],
               Obj1),
   (  var(Old_Value) -> true
   ;  obj_field(Obj, options_in, Options),
      (  Old_Value == Option
      -> domain_error(nonrepeating_options, Options)
      ;  domain_error(consistent_options, Options)
      )
   ).

'single_option_rule_v?'(Rule, options_object_out, Obj) :-
   obj_field(Rule, 'options_object_out#', Obj).

/*
'ur_options_v?'(Obj, is_meta_pred,
                That:is_meta_option(List)) :-
   context_module(That),
   obj_field(Obj, meta_options, List).

is_meta_option(List, Option) :- member(Option, List).
*/
