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

:- module(ur_option,
          [ur_options/2,
           options_object/3
           ]).

/** <module> Options processing
*/

:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(v(ur_options_v)).

:- meta_predicate ur_options(:, :).

%% ur_options(:Predicate, +Options)
%
% Define the options rule for Predicate
%
% Example:
% ==
% ur_options(random_string,
%           [multi(length, [length/2, length/1, empty/0], [empty, length(1, 80)]),
%            [group(pattern),
%             option(range/1),
%             option(regex/1),
%             meta_option(pattern/1),
%             default(range(32..126))],
%            [meta_option(generator/1),
%             default(generator(randgen:fd_random(lcq, gnu)))]
%           ]).
% ==

ur_options(Pred, _:Options) :-

   Ctx = context(ur_options/3, Details),
   strip_module(Pred, Pred_Module, Pred1),
   format(atom(Class), '~a__~a_v', [Pred_Module, Pred1]),
   ( class_name(Class) -> true
   ; db_clear(Class),
     catch(
           assert_rules(Options, Class, Ctx, Details),
           error(db_key_exists(_, _, New_Object), _),
           ( obj_key_value(New_Object, [Existing_Opt]),
             functor(Existing_Opt, Ex_Fun, Ex_Arity),
             format(atom(Details),
                 'multiple definitions for ~a/~d found',
                 [Ex_Fun, Ex_Arity]),
             Err =error(invalid_option_definition(Options),
                        Ctx),
             throw(Err)
           )),
     setof(Field, db_select(Class, [group_name], [Field]),
           Fields),
     class_create(Class, ur_options_v, Fields)
   ).

assert_rules([], _, _, _) :- !.
assert_rules([Rule0|T], DB, Ctx, Details) :-

   must_be(list, Rule0),
   Err = error(invalid_option_definition(Rule0), Ctx),
   assert_rule(Rule0, DB, Details, Err),
   assert_rules(T, DB, Details, Err).

% single group
assert_rule(Rule0, DB, Details, Err) :-

   % basic checks
   select_option(group(Name), Rule0, Rule1), !,
   % default value
   select_default(Rule1, Rule2, Default, Details, Err),
   assert_group_options(option(_), DB, Name, Default,
                        Rule2, Rule3, Details, Err),
   assert_group_options(meta_option(_), DB, Name, Default,
                        Rule3, Rule4, Details, Err),
   check_rest(Rule4, Details, Err),

   (  db_iterate(DB,
                 pattern(Default)
                /\ same_or_descendant(option_rule_v), _)
   -> true
   ;  Details = 'the default value is not belong to the group',
      throw(Err)
   ).

% option
assert_rule(Rule0, DB, Details, Err) :-

   % basic checks
   select_single_option(option(_), Rule0, Rule1,
                        Functor, Arity, Details, Err),
   !,
   (  option(meta_option(_), Rule1)
   -> Details = 'either option or meta_option should be specified, but not both',
      throw(Err)
   ;  true
   ),

   % default value
   select_default(Rule1, Rule2, Default, Details, Err),
   (  var(Default) -> true
   ;  functor(Default, Default_Functor, Default_Arity),
      (  Default_Functor == Functor,
         Default_Arity == Arity
      -> true
      ;  Details = 'invalid default value', throw(Err)
      )
   ),

   check_rest(Rule2, Details, Err),

   % assert
   functor(Pattern, Functor, Arity),
   db_construct(DB, single_option_rule_v,
                [group_name, pattern, is_meta,
                 default_value],
                [Functor, Pattern, false,
                 Default]).

% meta option
assert_rule(Rule0, DB, Details, Err) :-

   % basic checks
   select_single_option(meta_option(_), Rule0, Rule1,
                        Functor, Arity, Details, Err),
   !,
   (  option(option(_), Rule1)
   -> Details = 'either option or meta_option should be specified, but not both',
      throw(Err)
   ;  true
   ),
   (  Arity == 1 -> true
   ;  Details = 'for a meta option the only available arity is 1',
      throw(Err)
   ),

   % default value
   select_default(Rule1, Rule2, Default, Details, Err),
   (  var(Default) -> true
   ;  functor(Default, Default_Functor, Default_Arity),
      (  Default_Functor == Functor,
         Default_Arity == Arity
      -> true
      ;  Details = 'invalid default value', throw(Err)
      )
   ),
   check_rest(Rule2, Details, Err),

   % assert
   functor(Pattern, Functor, Arity),
   db_construct(DB, single_option_rule_v,
                [group_name, pattern, is_meta,
                 default_value],
                [Functor, Pattern, true,
                 Default]).

assert_rule(_, _, Details, Err) :-

   Details = 'either option or meta_option or group or multi_group should be specified',
   throw(Err).

select_single_option(Template, Rule0, Rule,
                     Functor, Arity, Details, Err) :-

   functor(Template, Name, 1),
   select_option(Template, Rule0, Rule),
   arg(1, Template, Option),
   (  nonvar(Option), Option = Functor/Arity,
      atom(Functor), integer(Arity), Arity >= 0
   -> true
   ;  format(atom(Details), '~a(Functor/Arity) expected',
             [Name]),
      throw(Err)
   ).

assert_group_options(Template0, DB, Group_Name, Default,
                     Rule0, Rule, Details, Err) :-

   copy_term(Template0, Template),
   select_single_option(Template, Rule0, Rule1,
                        Functor, Arity, Details, Err),
   !,
   (  functor(Template, meta_option, _)
   -> Is_Meta = true
   ;  Is_Meta = false
   ),
   functor(Pattern, Functor, Arity),
   db_construct(DB, group_option_rule_v,
                [group_name, pattern, is_meta,
                 default_value],
                [Group_Name, Pattern, Is_Meta,
                 Default]),
   assert_group_options(Template0, DB, Group_Name, Default,
                        Rule1, Rule, Details, Err).

assert_group_options(_, _, _, _, Rule, Rule, _, _).


select_default(Rule0, Rule, Default, Details, Err) :-

   (  select_option(default(Default), Rule0, Rule)
   -> (  compound(Default) -> true
      ;  Details = 'invalid default value', throw(Err)
      )
   ;  Rule = Rule0
   ).

check_rest([], _, _) :- !.
check_rest(_, Details, Err) :-
   Details = 'unknown parameters in the definition',
   throw(Err).

:- meta_predicate options_object(:, :, -).

options_object(Pred_Module:Pred, Opts_Module:Options, Object) :-
   format(atom(Class), '~a__~a_v', [Pred_Module, Pred]),
   obj_construct(Class,
                 [options_in, options_out, context_module],
                 [Options, Object, Opts_Module],
                 _).

% override_(+Option0, +Value, +Options, -Option)
%
% @param Option0 can have one of these forms:
%  $ default(Opt) : the default value for Opt
%  $ default_to_multi(List) : the default for muli-option
%  $ user(Opt) : user defined value for Opt
%  $ multi(List) : list of options of one group
%   (like [empty, length(4), length(6, 7)]).
%
% @param Value processed option (like length(1, 5))
%
% @param Options the full option list (for error reporting)
%
% @param Option will be one of [user(Opt), multi(List)]

% process opt(match(Val)) expression, Val will be unified with
% non-multy _(opt(Val)) option,
/*
override_(Prev_Stu, Stu, _, Prev_Stu) :-
	functor(Prev_Stu, _, 1),      %e.g. user(opt(Val))
	functor(Stu, _, 1),           %e.g. opt(match(Val))
	arg(1, Stu, match(Val)),
	!,
	arg(1, Prev_Stu, Opt),
	functor(Opt, _, 1),
	arg(1, Opt, Val).

override_(default_to_multi(_), Value, _, multi([Value])) :- !.
override_(multi([]), Value, _, multi([Value])) :- !.
override_(multi([V1|T]), Value, _, multi([Value, V1|T])) :- !.
*/












