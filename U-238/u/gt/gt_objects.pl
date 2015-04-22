% -*- fill-column: 65; -*-
:- module(gt_objects,
          [obj_fill_random/3,              % +Options0, -Options, -Obj
           obj_fill_random/4,              % +Options0, -Options,
                                           %   +Obj0, -Obj
           obj_fill_random_list/4,         % +Options0, -Options,
                                           %   +ObjList0, -ObjList
           obj_fill_downcast_random/4,     % +Options0, -Options,
                                           %   +Obj0, -Obj
           obj_fill_downcast_random_list/4 % +Options0, -Options,
                                           %   +ObjList0, -ObjList
          ]
         ).

:- use_module(library(assoc)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_option)).
:- use_module(u(v)).
:- use_module(u(logging)).
:- use_module(u(util/lambda)).

:- meta_predicate obj_fill_random(:, -, +).

obj_fill_random(Options0, Options, Obj) :-
   Ctx = context(obj_fill_random/3, _),
   obj_fill_random_cmn(Options0, Options, Obj, Obj, Ctx).
obj_fill_random(Options0, Options, Obj0, Obj) :-
   Ctx = context(obj_fill_random/4, _),
   obj_fill_random_cmn(Options0, Options, Obj0, Obj, Ctx).

obj_fill_random_cmn(Options0, Options, Obj0, Obj, Ctx) :-
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, _),
   functor(Obj0, Class, _),
   options_to_object(global:Class, Options0, Options1),
   obj_fill_random_int(Options1, Options, Obj0, Obj, Ctx).

obj_fill_random_int(Options0, Options, Obj0, Obj, _) :-
   obj_rewrite(Options0,
               [nested, context_module, global_options],
               [Assoc, Module, GlobalOpts0],
               [Assoc, Module, GlobalOpts],
               Options
               ),
   % Log assoc options defined for this level
   %(   var(Assoc) -> true % wouldn't log empty assocs
   %;   assoc_to_keys(Assoc, AssocKeys)
       %LogOpts ^= GlobalOpts0 / log_options,
       %log_piece(['assoc-options:'|AssocKeys], LogOpts)
   %),
   % Select options matching object fields. Skip evals.
   findall_fields(\_^_^T^nonvar(T), Obj0, _, false, Vs),
   obj_fill_random_req(Vs, Assoc, Module, GlobalOpts0, GlobalOpts,
                       Obj0, Obj).

obj_fill_random_req([], _, _, GO, GO, Obj, Obj) :- !.
obj_fill_random_req([v(Name,Value0,Type)|T], Assoc, Module, GO0, GO,
                    Obj0, Obj)
:-
   (  nonvar(Assoc), get_assoc(Name, Assoc, O0)
   -> copy_term(O0, O1, _)
   ;  O1 = []
   ),
   options_to_object(global:Name, Module:O1, O2),
   obj_field(O2, global_options, GO0),
   % process gtrace option
   obj_field(O2, gtrace, GTrace),
   (  nonvar(GTrace) -> gtrace ; true ),
   % Replace the value of the field.
   % We always do replays to allow downcast of modified sub-values
   obj_rewrite(Obj0, [Name], [Value0], [Value1], Obj1),
   term_attvars(Value0, V0A), length(V0A, V0N),
   objects:value_set(Type, Module:O2, Module:O, Value0, Value1),
   term_attvars(Value1, V1A), length(V1A, V1N),
   ( V0N =\= V1N -> gtrace ; true),
   obj_field(O, global_options, GO1),
   obj_fill_random_req(T, Assoc, Module, GO1, GO, Obj1, Obj).

:- meta_predicate obj_fill_downcast_random(:, -, +, -).

obj_fill_downcast_random(Options0, Options, Obj0, Obj) :-
   Ctx = context(obj_fill_downcast_random/4, _),
   obj_fill_downcast_random_cmn(Options0, Options, Obj0,
                                Obj, Ctx).

obj_fill_downcast_random_cmn(Options0, Options, Obj0, Obj, Ctx) :-
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, _),
   functor(Obj0, Class, _),
   options_to_object(global:Class, Options0, Options1),
   obj_fill_downcast_random_int(Options1, Options, Obj0, Obj, Ctx).

obj_fill_downcast_random_int(Options0, Options, Obj0, Obj, Ctx) :-
    % fill
    obj_fill_random_int(Options0, Options1, Obj0, Obj1, Ctx),
    % downcast
    obj_downcast(Obj1, Obj2),
    (  Obj1 == Obj2
    -> Obj = Obj2,  % no more downcasts
       Options1 = Options
    ;  Options1 / global_options / log_options ^= LogOpts,
       functor(Obj1, Class1, _),
       functor(Obj2, Class2, _),
       log_piece(['DC', Class1, '->', Class2], LogOpts),
       obj_fill_downcast_random_int(Options1, Options, Obj2, Obj, Ctx)
    ).

:- meta_predicate obj_fill_downcast_random_list(:, -, +, -).

%% obj_fill_random_list(+Options0, -Options, +ObjList0, -ObjList) is nondet.
%
%  Works like obj_fill_random/4 for all members
%  of ObjList and passing Options as Options0 to the next
%  call.
%  It is semidet/nondet depending on the options.
%
obj_fill_random_list(Options0, Options, ObjList0, ObjList) :-
   Ctx = context(obj_fill_random_list/4, _),
   obj_fill_random_list_int(Options0, Options, _, ObjList0, ObjList, Ctx).

obj_fill_random_list_int(Options, Options, _, [], [], _):- !.
obj_fill_random_list_int(Options0, Options, OptionsClass, [Obj0|T0], [Obj|T], Ctx) :-
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, Class_Id),
   (  nonvar(OptionsClass)
   -> Options1 = Options0
   ;  class_id(Class_Id, OptionsClass),
      options_to_object(global:OptionsClass, Options0, Options1)
      % NB only first object class used
   ),
   obj_fill_random_int(Options1, Options2, Obj0, Obj, Ctx),
   obj_fill_random_list_int(Options2, Options, OptionsClass, T0, T, Ctx).

%% obj_fill_downcast_random_list(+Options0, -Options, +ObjList0, -ObjList) is nondet.
%
%  Works like obj_fill_downcast_random/4 for all members
%  of ObjList0 and passing Options as Options0 to the next
%  call.
%  It is semidet/nondet depending on options.
%
obj_fill_downcast_random_list(Options0, Options, ObjList0, ObjList) :-
   Ctx = context(obj_fill_downcast_random_list/4, _),
   obj_fill_downcast_random_list_int(1, Options0, Options, _, ObjList0, ObjList, Ctx).

obj_fill_downcast_random_list_int(_, Options, Options, _, [], [], _):- !.
obj_fill_downcast_random_list_int(I, Options0, Options, OptionsClass, [Obj0|T0],
                                  [Obj|T], Ctx) :-
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, _),
   (  nonvar(OptionsClass)
   -> Options1 = Options0
   ;  functor(Obj0, OptionsClass, _),
      options_to_object(global:OptionsClass, Options0, Options1)
      % NB only first object class used
   ),
   Options1 / global_options / log_options ^= LogOpts,
   log_piece([I, ']'], LogOpts),
   obj_fill_downcast_random_int(Options1, Options2, Obj0, Obj, Ctx),
   succ(I, I1),
   obj_fill_downcast_random_list_int(I1, Options2, Options, OptionsClass, T0, T, Ctx).

