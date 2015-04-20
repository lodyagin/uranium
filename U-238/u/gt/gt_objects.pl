:- module(gt_objects,
          [obj_fill_random/3,              % +Options0, -Options, +Obj
           obj_fill_random_list/3,         % +Options0, -Options, +ObjList
           obj_fill_downcast_random/4,     % +Options0, -Options, +Obj0, -Obj
           obj_fill_downcast_random_list/4 % +Options0, -Options, +ObjList0, -ObjList
          ]
         ).

:- use_module(library(assoc)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_option)).
:- use_module(u(v)).
:- use_module(u(logging)).

:- meta_predicate obj_fill_random(:, -, +).

obj_fill_random(Options0, Options, Obj) :-
   Ctx = context(obj_fill_random/3, _),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   class_id(Class_Id, Class),
   options_to_object(global:Class, Options0, Options1),
   obj_fill_random_int(Options1, Options, Class_Id, Obj, Ctx).

obj_fill_random_int(Options0, Options, Class_Id, Obj, Ctx) :-
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
   % Select options matching object fields
   findall(v(Name,Value,Type),
           ( obj_field_int(Class_Id, Name, throw, Obj,
                           Value, Type, Ctx),
             nonvar(Type)
           ),
           Vs),
   obj_fill_random_req(Vs, Assoc, Module, GlobalOpts0, GlobalOpts, Obj).

obj_fill_random_req([], _, _, GO, GO, _) :- !.
obj_fill_random_req([v(Name,Value,Type)|T], Assoc, Module, GO0, GO, Obj) :-
   (  nonvar(Assoc), get_assoc(Name, Assoc, O0)
   -> copy_term(O0, O1, _)
   ;  O1 = []
   ),
   options_to_object(global:Name, Module:O1, O2),
   obj_field(O2, global_options, GO0),
   % process gtrace option
   obj_field(O2, gtrace, GTrace),
   (  nonvar(GTrace) -> gtrace ; true ),
   % set the value of the field
   objects:value_set(Type, Module:O2, Module:O, Value),
   obj_field(O, global_options, GO1),
   obj_field(Obj, Name, Value),
   obj_fill_random_req(T, Assoc, Module, GO1, GO, Obj).

:- meta_predicate obj_fill_downcast_random(:, -, +, -).

obj_fill_downcast_random(Options0, Options, Obj0, Obj) :-
   Ctx = context(obj_fill_downcast_random/4, _),
   obj_fill_downcast_random_cmn(Options0, Options, Obj0,
                                Obj, Ctx).

obj_fill_downcast_random_cmn(Options0, Options, Obj0, Obj, Ctx) :-
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, Class_Id),
   class_id(Class_Id, Class),
   options_to_object(global:Class, Options0, Options1),
   obj_fill_downcast_random_int(Options1, Options, Class_Id, Obj0, Obj, Ctx).

obj_fill_downcast_random_int(Options0, Options, Class_Id, Obj0, Obj, Ctx) :-
    % fill
    obj_fill_random_int(Options0, Options1, Class_Id, Obj0, Ctx),
    % downcast
    obj_downcast(Obj0, Obj1),
    (  Obj0 == Obj1
    -> Obj = Obj1,  % no more downcasts
       Options1 = Options
    ;  Options1 / global_options / log_options ^= LogOpts,
       functor(Obj0, Class0, _),
       functor(Obj1, Class1, _),
       log_piece(['DC', Class0, '->', Class1], LogOpts),
       obj_class_id(Obj1, Class_Id1),
       obj_fill_downcast_random_int(Options1, Options, Class_Id1, Obj1, Obj, Ctx)
    ).

:- meta_predicate obj_fill_downcast_random_list(:, -, +, -).

%% obj_fill_random_list(+Options0, -Options, +ObjList) is nondet.
%
%  Works like obj_fill_random/3 for all members
%  of ObjList and passing Options as Options0 to the next
%  call.
%  It is semidet/nondet depending on options.
%
obj_fill_random_list(Options0, Options, ObjList) :-
   Ctx = context(obj_fill_random_list/3, _),
   obj_fill_random_list_int(Options0, Options, _, ObjList, Ctx).

obj_fill_random_list_int(Options, Options, _, [], _):- !.
obj_fill_random_list_int(Options0, Options, OptionsClass, [Obj|T], Ctx) :-
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   (  nonvar(OptionsClass)
   -> Options1 = Options0
   ;  class_id(Class_Id, OptionsClass),
      options_to_object(global:OptionsClass, Options0, Options1)
      % NB only first object class used
   ),
   obj_fill_random_int(Options1, Options2, Class_Id, Obj, Ctx),
   obj_fill_random_list_int(Options2, Options, OptionsClass, T, Ctx).

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
   check_object_arg(Obj0, Ctx, Class_Id),
   (  nonvar(OptionsClass)
   -> Options1 = Options0
   ;  class_id(Class_Id, OptionsClass),
      options_to_object(global:OptionsClass, Options0, Options1)
      % NB only first object class used
   ),
   Options1 / global_options / log_options ^= LogOpts,
   log_piece([I, ']'], LogOpts),
   obj_fill_downcast_random_int(Options1, Options2, Class_Id, Obj0, Obj, Ctx),
   succ(I, I1),
   obj_fill_downcast_random_list_int(I1, Options2, Options, OptionsClass, T0, T, Ctx).

