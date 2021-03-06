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
          [extract_weak/5,       % :Pred, +Options0, -Options, -Weak, +Ctx
           options_group_list/3, % :Pred, +GroupName, -List
           options_object/3,     % :Pred, +Options, -Object
           options_object/5,     % :Pred, +Options, ?Weak0, ?UseDefaults0,
                                 % -Object
           options_predicate_to_options_class_name/2,
           options_to_object/3,  % :Pred, +Options, -Opt
           options_to_object/4,  % :Pred, +Options, +Weak, -Opt
           overwrite_options/4,  % +Old, +New, +AddOverwriteFields,
                                 % -NewOpts
           retract_options/1,    % :Pred
           ur_options/2          % :Pred, +Options
           %ur_options_clear/1   % :Pred
           ]).

/** <module> Options processing
*/

:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(util/lambda)).
:- use_module(u(ur_enums)).
:- use_module(u(internal/decode_arg)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(v(ur_options_v)).


:- meta_predicate extract_weak(:, :, -, -, +).

%% extract_weak(:Pred, +Options0, -Options, -Weak, +Ctx) is det.
%
% Extract a how_weak options group. As a side effect make sure that
% Options is an option object corresponding to Options0.  @see
% std_weak_arg_values/1
%
extract_weak(Pred, Options0, Options, Weak, Ctx) :-
   options_to_object(Pred, Options0, Options),
   obj_field(Options, how_weak, Weak1),
   std_weak_arg_values(LOL),
   decode_arg(LOL, Weak1, Weak, Ctx).

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
   atom(Pred), !,
   format(atom(Class), '~a__~a_v', [Module, Pred]).
options_predicate_to_options_class_name(Module:Pred0, Class) :-
   functor(Pred0, Pred, _),
   options_predicate_to_options_class_name(Module:Pred, Class).


%% overwrite_options(+Old, +New, +AddOverwriteFields, -NewOpts) is det.
%
% Overwrite options values from Old by all bound values from New. Use
% only options introduced in New class + AddOverwriteFields
%
overwrite_options(Old, New, AddOverwriteFields, NewOpts) :-
   findall_fields( \_^V^_^(nonvar(V), V \=@= [_]), % skip also
                                                   % not initialized
                                                   % options multi_groups
                   New, true, false, Vs1),
   findall( v(F, V, T),
	    ( member(F, AddOverwriteFields),
	      named_arg(New, F, V, T),
	      nonvar(V) ),
	    Vs2 ),
   append(Vs1, Vs2, Vs),
   (   setof(F, V^T^member(v(F, V, T), Vs), NewFields1) -> true; NewFields1 = []),
   obj_combine(Old, New, NewFields1, NewOpts).

% C are combined fields from A and B.
obj_combine(A, B, B_Fields, C) :-
   obj_unify(B, B_Fields, B_Values),
   obj_rewrite(A, B_Fields, _, B_Values, C).

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

%:- meta_predicate ur_options_clear(:).

%ur_options_clear(Pred) :-
%   options_predicate_to_options_class_name(Pred, Class),

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
   -> (  (compound(Default);atom(Default)) -> true
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
   options_object(Pred, M:Options, Weak, true, Object).


:- meta_predicate options_object(:, :, -).
:- meta_predicate options_object(:, :, ?, +, -).

options_object(Pred, Options, Object) :-
   options_object_cmn(Pred, Options, strict, true, Object).

%% options_object(:Pred, +Options, ?Weak0, ?UseDefaults0, -Object) is det.
options_object(Pred, Options, Weak0, UseDefaults0, Object) :-
   Ctx = context(options_object/5, _),
   decode_arg([[strict],
               [_, weak]],
              Weak0, Weak, Ctx),
   decode_arg([[true, _, t],
               [false, fail, f]],
               UseDefaults0, UseDefaults, Ctx),
   options_object_cmn(Pred, Options, Weak, UseDefaults, Object).

options_object_cmn(Pred_Module:Pred, Opts_Module:Options, Weak,
                   UseDefaults, Object) :-
   memberchk(UseDefaults-IgnoreDefaults, [true-_, false-ignore]),
   options_predicate_to_options_class_name(Pred_Module:Pred, Class),
   obj_construct(Class,
      [options_in, options_out, context_module, weak, ignore_defaults],
      [Options, Object, Opts_Module, Weak, IgnoreDefaults],
      _).

:- initialization clear_decode_arg.









