:- module(gt_objects,
          [obj_fill_random/2,
           obj_fill_downcast_random/3]
         ).

:- use_module(library(assoc)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

obj_fill_random(Options, Obj) :-
   Ctx = context(obj_fill_random/2, _),
   check_object_arg(Options, Ctx, _),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   obj_fill_random_int(Options, Class_Id, Obj, Ctx).

obj_fill_random_int(Options, Class_Id, Obj, Ctx) :-
   findall(v(Name,Value,Type),
           ( obj_field_int(Class_Id, Name, throw, Obj,
                           Value, Type, Ctx),
             var(Value),
             nonvar(Type)
           ),
           Vs),
   foreach(member(v(Name,Value,Type), Vs),
           ignore(( objects:value_set(Type, Options, Value),
                    obj_field(Obj, Name, Value)
                 ))
          ).


obj_fill_downcast_random(Options, Obj0, Obj) :-
   Ctx = context(obj_fill_downcast_random/3, _),
   check_object_arg(Options, Ctx, _),
   check_inst(Obj0, Ctx),
   check_object_arg(Obj0, Ctx, Class_Id),
   obj_fill_downcast_random_int(Options, Class_Id, Obj0, Obj, Ctx).

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

