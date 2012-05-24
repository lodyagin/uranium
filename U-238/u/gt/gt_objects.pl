:- module(gt_objects,
          [obj_fill_random/2]
         ).

:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_option)).
:- use_module(u(v)).

:- meta_predicate obj_fill_random(:, ?).

obj_fill_random(Options, Obj) :-

   Ctx = context(obj_fill_random/2, _),
   check_inst(Obj, Ctx),
   check_object_arg(Obj, Ctx, Class_Id),
   
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
