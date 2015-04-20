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
          [options_group_list/3, % :Pred, +GroupName, -List
           ur_options/2,
           options_object/3,
           options_object/4,
           options_predicate_to_options_class_name/2,
           options_to_object/3,  % :Pred, +Options, -Opt
           options_to_object/4,  % :Pred, +Options, +Weak, -Opt
           retract_options/1     % :Pred
           ]).

/** <module> Options processing
*/

:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(v(ur_options_v)).


:- meta_predicate options_group_list(:, +, -).

%% options_group_list(:Pred, ?GroupName, -List) is nondet.
%
% If Pred has an option with {multi_}group GroupName returns all
% members of that group as a List.
%
options_group_list(Pred, GroupName, List) :-
   (  Pred = _:class(Class) -> true
   ; options_predicate_to_options_class_name(Pred, Class)
   ),
   bagof(E,
         O^( db_iterate(Class, group_name(GroupName), O),
           obj_field(O, pattern, E) ),
         List).

:- meta_predicate options_predicate_to_options_class_name(:, -).

% Defines the option class naming rule
options_predicate_to_options_class_name(Module:Pred, Class) :-
   format(atom(Class), '~a__~a_v', [Module, Pred]).

:- meta_predicate retract_options(:).

%% retract_options(:Pred) is det.
%
% Remove all options registered for Pred.
%
retract_options(Pred) :-
   options_predicate_to_options_class_name(Pred, Class),
   db_clear(Class).

:- meta_predicate ur_options(:, :).

%% ur_options(:Predicate, +Options)
%
% Define the options rule for Predicate
%
% Example:
% ==
% ur_options(random_string,
%           [[multi_group(length),
%             option(length/2),
%             option(length/1),
%             option(empty/0)],
%             default([empty, length(1, 80)])],
%            [group(pattern),
%             option(range/1),
%             option(regex/1),
%             meta_option(pattern/1),
%             default(range(32..126))],
%            [meta_option(generator/1),
%             default(generator(randgen:fd_random(lcq, gnu)))]
%           ]).
% ==

ur_options(Pred, _:Options0) :-
   Ctx = context(ur_options/2, Details),
   options_predicate_to_options_class_name(Pred, Class),
   % Add default options
   Options = [[group(gtrace), option(gtrace/0)] | Options0],
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
     (  setof(Field, db_select(Class, [group_name], [Field]),
              Fields1)
     -> Fields = Fields1
     ;  Fields = []
     ),
     class_primary_id(ur_options_v, Ur_Options_Class_Id),
     class_fields(_, Ur_Options_Class_Id, _, _, Parent_Fields),
     ord_intersection(Parent_Fields, Fields, Overriding),
     (  Overriding == [] -> true
     ;  format(atom(Details),
               'the option name(s) ~p override ur_options_v class fields',
               [Overriding]),
        throw(error(invalid_option_definition(Options), Ctx))
     ),
     class_create(Class, ur_options_v, Fields)
   ),
   % Assert enums if group(list) exists
   (  options_group_list(class(Class), list, Enums)
   -> assert_enum(Class:Enums)
   ;  true
   ).

assert_rules([], _, _, _) :- !.
assert_rules([Rule0|T], DB, Ctx, Details) :-

   must_be(list, Rule0),
   Err = error(invalid_option_definition(Rule0), Ctx),
   assert_rule(Rule0, DB, Details, Err),
   assert_rules(T, DB, Ctx, Details).

% single group
assert_rule(Rule0, DB, Details, Err) :-

   % basic checks
   select_option(group(Name), Rule0, Rule1), !,
   % default value
   select_default(Rule1, Rule2, Default, Details, Err),
   assert_group_options(DB, option(_),
                        group_option_rule_v, Name,
                        Default, Rule2, Rule3,
                        Details, Err),
   assert_group_options(DB, meta_option(_),
                        group_option_rule_v, Name,
                        Default, Rule3, Rule4,
                        Details, Err),
   check_rest(Rule4, Details, Err),

   (  db_iterate(DB,
                 pattern(Default)
                /\ same_or_descendant(option_rule_v), _)
   -> true
   ;  Details = 'the default value is not belong to the group',
      throw(Err)
   ).

% multi group
assert_rule(Rule0, DB, Details, Err) :-

   % basic checks
   select_option(multi_group(Name), Rule0, Rule1), !,
   % default value
   select_default(Rule1, Rule2, Default, Details, Err),
   ( is_list(Default) -> true
   ; Details = 'the default value for multi_group must be a list',
     throw(Err)
   ),
   assert_group_options(DB, option(_),
                        multi_group_option_rule_v, Name,
                        Default, Rule2, Rule3,
                        Details, Err),
   assert_group_options(DB, meta_option(_),
                        multi_group_option_rule_v, Name,
                        Default, Rule3, Rule4,
                        Details, Err),
   check_rest(Rule4, Details, Err),

   findall(Pattern,
           db_iterate(DB,
                    pattern(Pattern)
                   /\ same_or_descendant(option_rule_v),
                    _),
         Patterns),
   (  forall(member(O, Default), memberchk(O, Patterns))
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

assert_group_options(DB, Template0, Rule_Class,
                     Group_Name, Default,
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
   db_construct(DB, Rule_Class,
                [group_name, pattern, is_meta,
                 default_value],
                [Group_Name, Pattern, Is_Meta,
                 Default]),
   assert_group_options(DB, Template0, Rule_Class,
                        Group_Name, Default,
                        Rule1, Rule, Details, Err).

assert_group_options(_, _, _, _, _, Rule, Rule, _, _).


select_default(Rule0, Rule, Default, Details, Err) :-

   (  select_option(default(Default), Rule0, Rule)
   -> (  compound(Default) -> true
      ;  Details = 'invalid default value', throw(Err)
      )
   ;  Rule = Rule0
   ).

check_rest([], _, _) :- !.
check_rest(Rejected, Details, Err) :-
   format(atom(Details),
          'unknown parameters ~w in the definition',
          [Rejected]),
   throw(Err).

:- meta_predicate options_to_object(:, :, -).
:- meta_predicate options_to_object(:, :, +, -).

%% options_to_object(:Pred, +Options, -Opt) is det.
%
% Converts Options to options object if needed
%
options_to_object(Pred, Options, Opt) :-
   options_to_object(Pred, Options, strict, Opt).

%% options_to_object(:Pred, +Options, +Weak, -Opt) is det.
%
% Converts Options to options object if needed
%
options_to_object(_, _:Object, _, Object) :-
   u_object(Object), !.
options_to_object(Pred, M:Options, Weak, Object) :-
   must_be(list, Options),
   options_object(Pred, M:Options, Weak, Object).


:- meta_predicate options_object(:, :, -).

options_object(Pred, Options, Object) :-
   options_object_cmn(Pred, Options, strict, Object).

options_object(Pred, Options, Weak0, Object) :-
   Ctx = context(options_object/4, _),
   decode_arg([[strict],
               [_, weak]],
              Weak0, Weak, Ctx),
   options_object_cmn(Pred, Options, Weak, Object).

options_object_cmn(Pred_Module:Pred, Opts_Module:Options, Weak, Object) :-
   options_predicate_to_options_class_name(Pred_Module:Pred, Class),
   obj_construct(Class,
                 [options_in, options_out, context_module, weak],
                 [Options, Object, Opts_Module, Weak],
                 _).

% TODO check all this functionality is realized in the new module ur_option:

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


:- initialization clear_decode_arg.









