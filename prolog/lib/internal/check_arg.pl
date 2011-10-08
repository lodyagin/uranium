:- module(check_arg,
          [check_inst/2,
           check_class_arg/2,
           check_object_arg/3
           ]).

:- use_module(objects_i).

check_inst(Arg, Ctx) :-

   (  var(Arg)
   -> throw(error(instantiation_error, Ctx))
   ;  true
   ).
  
check_class_arg(Class, Err_Context) :-

   nonvar(Class),
   (  \+ atom(Class)
   -> throw(error(type_error(atom, Class), Err_Context))
   ;  u_class(Class)
   -> true
   ;  throw(error(domain_error(uranium_class, Class),Err_Context))
   ).

check_object_arg(Object, Err_Context, Class_Id) :-

   nonvar(Object),
   (  \+ u_object(Object)
   -> throw(error(type_error(uranium_object, Class), Err_Context))
   ;  obj_class_id(Object, Class_Id),
      integer(Class_Id),
      functor(Object, Class, _),
      class_id(Class_Id, Class)
   -> true
   ;  throw(invalid_object(Object, 'invalid class id'))
   ).


