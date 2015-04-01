:- module(gt_objects,
          [obj_fill_random/2,
           obj_fill_downcast_random/3]
         ).

:- use_module(library(assoc)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

:- meta_predicate obj_fill_random(:, ?).

obj_fill_random(Options, Obj) :-
   Ctx = context(obj_fill_random/2, _),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   class_id(Class_Id, Class),
   options_to_object(global:Class, Options, Opt),
   obj_fill_random_int(Opt, Class_Id, Obj, Ctx).

obj_fill_random_int(Options, Class_Id, Obj, Ctx) :-
   obj_unify(Options, [nested, context_module], [Assoc, Module]),
   findall(v(Name,Value,Type),
           ( obj_field_int(Class_Id, Name, throw, Obj,
                           Value, Type, Ctx),
             var(Value),
             nonvar(Type)
           ),
           Vs),
   obj_fill_random_req(Vs, Assoc, Module, Obj).

obj_fill_random_req([], _, _, _) :- !.
obj_fill_random_req([v(Name,Value,Type)|T], Assoc, Module, Obj) :-
   (  nonvar(Assoc), get_assoc(Name, Assoc, O)
   -> objects:value_set(Type, Module:O, Value)
   ;  objects:value_set(Type, [], Value)
   ),
   obj_field(Obj, Name, Value),
   obj_fill_random_req(T, Assoc, Module, Obj).

:- meta_predicate obj_fill_downcast_random(:, +, -).

obj_fill_downcast_random(Options, Obj0, Obj) :-
   Ctx = context(obj_fill_downcast_random/3, _),
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, Class_Id),
   class_id(Class_Id, Class),
   options_to_object(global:Class, Options, Opt),
   obj_fill_downcast_random_int(Opt, Class_Id, Obj0, Obj, Ctx).

obj_fill_downcast_random_int(Options, Class_Id, Obj0, Obj, Ctx) :-
    % fill
    obj_fill_random_int(Options, Class_Id, Obj0, Ctx),
    % downcast
    obj_downcast(Obj0, Obj1),
    (  Obj0 == Obj1
    -> Obj = Obj1  % no more downcasts
    ;  obj_class_id(Obj1, Class_Id1),
       obj_fill_downcast_random_int(Options, Class_Id1, Obj1, Obj, Ctx)
    ).

