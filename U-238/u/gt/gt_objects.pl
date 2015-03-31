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

obj_fill_random(That:Options, Obj) :-

   Ctx = context(obj_fill_random/2, _),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),

   options_to_assoc(Options, Opt_Type, Options1),
   
   findall(v(Name,Value,Type),
           ( obj_field_int(Class_Id, Name, throw, Obj,
                           Value, Type, Ctx),
             var(Value),
             nonvar(Type)
           ),
           Vs),

   (  Opt_Type == list
   -> foreach(member(v(Name,Value,Type), Vs),
              ignore(( objects:value_set(Type, Options1, Value),
                       obj_field(Obj, Name, Value)
                    ))
             )
   ;  % assoc
      foreach(member(v(Name,Value,Type), Vs),
              ignore(( (  get_assoc(Name, Options1, O)
                       -> objects:value_set(Type, That:O, Value)
                       ;  objects:value_set(Type, That:Options1, Value)
                       ),
                       obj_field(Obj, Name, Value)
                    ))
             )
   ).


:- meta_predicate obj_fill_downcast_random(:, +, -).

obj_fill_downcast_random(Options_List, Obj0, Obj) :-
    options_to_assoc(Options_List, _, Options_List1),
    obj_fill_downcast_random_int(Options_List1, Obj0, Obj).

obj_fill_downcast_random_int(Options_List, Obj0, Obj) :-
    % fill
    obj_fill_random(Options_List, Obj0),
    % downcast
    obj_downcast(Obj0, Obj1),
    (  Obj0 == Obj1
    -> Obj = Obj1  % no more downcasts
    ;  obj_fill_downcast_random_int(Options_List, Obj1, Obj)
    ).

