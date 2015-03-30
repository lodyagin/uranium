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


is_assoc_fast(t) :- !.
is_assoc_fast(t(_,_,_,_,_)).

is_list_fast([]) :- !.
is_list_fast([_|_]).

% Fills then downcasts till no more downcasts
obj_fill_downcast_random(Options_List, Obj0, Obj) :-
    is_assoc_fast(Options_List), !,
    obj_fill_downcast_random_int(Options_List, Obj0, Obj).

obj_fill_downcast_random(Options_List, Obj0, Obj) :-
    is_list_fast(Options_List), !,
    list_to_assoc(Options_List, Options_List1),
    obj_fill_downcast_random_int(Options_List1, Obj0, Obj).

obj_fill_downcast_random_int(Options_List, Obj0, Obj) :-
    functor(Obj0, New_Class, _),
    % get options for the new object
    (  get_assoc(New_Class, Options_List, Options) -> true
    ;  Options = []
    ),
    % fill
    obj_fill_random(Options, Obj0),
    % downcast
    obj_downcast(Obj0, Obj1),
    (  Obj0 == Obj1
    -> Obj = Obj1  % no more downcasts
    ;  obj_fill_downcast_random_int(Options_List, Obj1, Obj)
    ).

