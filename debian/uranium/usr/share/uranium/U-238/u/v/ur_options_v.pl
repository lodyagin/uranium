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

new_class(ur_options_v, object_v,
          [options_in,     % as passed by a user
           context_module, % the context module for meta-options
           weak            % `strict` or whatever
          ]
         ).

new_class(gt_strings__random_string_options_v,
          ur_options_v,
          [
           ]).

new_class(option_rule_v, db_object_v,
          [group_name,
           pattern,
           options_object_in,
           'options_object_out#',
           is_meta,  % `pattern` is a meta option
           default_value
           ],
          [pattern]).

new_class(single_option_rule_v, option_rule_v, []).

new_class(group_option_rule_v, option_rule_v, []).

new_class(multi_group_option_rule_v, option_rule_v, []).

'ur_options_v?'(Obj0, options_out, Obj) :-

   obj_unify(Obj0, [class, options_in, weak], [Class, Options, Weak]),
   process_options(Options, Class, Weak, Obj0, Obj),
   set_defaults(Class, Obj).

set_defaults(DB, Obj) :-

   foreach(
           db_iterate(DB,
                      default_value(+bound)
                     /\ default_value(Default_Value)
                     /\ group_name(Name),
                      _),

           ignore(obj_field(Obj, Name, Default_Value))
          ).

process_options([], _, _, Obj, Obj) :- !.
process_options([Option|T], DB, Weak, Obj0, Obj) :-
   must_be(nonvar, Option),
   (  db_iterate(DB,
                 pattern(Option)
                /\ same_or_descendant(option_rule_v),
                 Rule)
   -> obj_unify(Rule,
                [options_object_in, option_in,
                 options_object_out],
                [Obj0, Option,
                 Obj1])
   ;
      (  Weak == strict
      -> domain_error(valid_option, Option)
      ;  Obj1 = Obj0 % ignore unknown options
      )
   ),
   process_options(T, DB, Weak, Obj1, Obj).

'option_rule_v?'(Rule, options_object_out, Obj) :-
   obj_field(Rule, 'options_object_out#', Obj).

% It will be called iff others fail
'option_rule_v?'(Rule, option_in, _) :-
   Ctx = context('option_rule_v?'/3, _),
   throw(error(abstract_eval(Rule, option_in), Ctx)).

'single_option_rule_v?'(Rule, option_in, Option0) :-
   obj_unify(Rule,
             [group_name,
              options_object_in,
              'options_object_out#',
              is_meta],
             [Name,
              Obj,
              Obj1,
              Is_Meta]),
   normalize_meta_option(Is_Meta, Context_Module,
                         Option0, Option),
   obj_rewrite(Obj,
               [Name, context_module],
               [Old_Value, Context_Module],
               [Option, Context_Module],
               Obj1),
   (  var(Old_Value) -> true
   ;  obj_field(Obj, options_in, Options),
      (  Old_Value == Option
      -> domain_error(nonrepeating_options, Options)
      ;  domain_error(consistent_options, Options)
      )
   ).

'group_option_rule_v?'(Rule, option_in, Option0) :-
   obj_unify(Rule,
             [group_name,
              options_object_in,
              'options_object_out#',
              is_meta],
             [Name,
              Obj,
              Obj1,
              Is_Meta]),
   must_be(nonvar, Is_Meta),
   normalize_meta_option(Is_Meta, Context_Module,
                         Option0, Option),
   obj_rewrite(Obj,
               [Name, context_module],
               [Old_Value, Context_Module],
               [Option, Context_Module],
               Obj1),
   (  var(Old_Value) -> true
   ;  obj_field(Obj, options_in, Options),
      (  Old_Value == Option
      -> domain_error(nonrepeating_options, Options)
      ;  functor(Old_Value, Option_Name, Option_Arity),
         functor(Option, Option_Name, Option_Arity)
      -> domain_error(consistent_options, Options)
      ;  domain_error(one_option_per_group, Options)
      )
   ).

'multi_group_option_rule_v?'(Rule, option_in, Option0) :-
   obj_unify(Rule,
             [group_name,
              options_object_in,
              'options_object_out#',
              is_meta],
             [Name,
              Obj,
              Obj1,
              Is_Meta]),
   must_be(nonvar, Is_Meta),
   normalize_meta_option(Is_Meta, Context_Module,
                         Option0, Option),
   obj_rewrite(Obj,
               [Name, context_module],
               [Old_Value, Context_Module],
               [New_Value, Context_Module],
               Obj1),
   (  var(Old_Value)
   -> New_Value = [Option]
   ;  New_Value = [Option|Old_Value]
   ).

normalize_meta_option(true, Context_Module,
                      Option0, Option) :- !,
   functor(Option0, Name, 1),
   arg(1, Option0, Value0),
   (  Value0 = _:_
   -> Value = Value0
   ;  Value = Context_Module : Value0
   ),
   functor(Option, Name, 1),
   arg(1, Option, Value).

normalize_meta_option(_, _, Option, Option).


