:- module(check_arg,
          [
           check_class_arg/2,
           check_db_key/2,             % inc. inst (inst+)
           check_existing_class_arg/2,
           check_existing_class_arg/3,
           check_field_name/2, 
           check_fields_arg/2,         % not inc. inst (inst-)
           check_inst/2,
           check_list_fast_arg/2,
           check_values_arg/3, % +Field_List, +Value_List, +Ctx
                               % inst-
           
           check_object_arg/3,         % not inc. inst
           check_rebase_rule/2,

           decode_arg/4,  % +Vals_LOL, +Arg_Val, -Result, +Ctx
           clear_decode_arg
           ]).

:- use_module(objects_i).
:- use_module(db_i).
:- use_module(u(ur_lists)).

:- dynamic arg_decode/4.

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

check_existing_class_arg(Class, Ctx) :-

   check_existing_class_arg(Class, Ctx, _).

check_existing_class_arg(Class, Ctx, Class_Id) :-

   check_class_arg(Class, Ctx),
   (  class_primary_id(Class, Class_Id) -> true
   ;  throw(error(existence_error(uranium_class, Class), Ctx))
   ).

check_db_key(DB_Key, Ctx) :-

   check_inst(DB_Key, Ctx),
   (  db_key_is_valid(DB_Key)
   -> true
   ;  throw(error(domain_error(db_key, DB_Key), Ctx))
   ).

check_fields_arg(Field_Names, Ctx) :-

   check_field_names(Field_Names, Field_Names, Ctx).

check_field_names([], _, _) :- !.

check_field_names([Field_Name|T], Full, Ctx) :-

   check_field_name(Field_Name, Ctx),
   check_field_names(T, Full, Ctx), !.

check_field_names(_, Full, Ctx) :-

   throw(error(type_error(list, Full), Ctx)).

check_field_name(Field_Name, Ctx) :-

   (  var(Field_Name)
   -> throw(error(instantiation_error, Ctx))
   ;  \+ atom(Field_Name)
   -> throw(error(type_error(atom, Field_Name), Ctx))
   ;  true
   ).

check_list_fast_arg(List, Ctx) :-

   (  (List = []; List = [_|_])
   -> true
   ;  throw(error(type_error(list, List), Ctx))
   ).

check_values_arg(Field_List, Value_List, Ctx) :-

   nonvar(Value_List),
   ( \+ is_list(Value_List)
   -> throw(error(type_error(list, Value_List), Ctx))
   ;  length(Field_List, LL), length(Value_List, LL)
   -> true
   ;  throw(error(domain_error(matched_list_length,
                  (Field_List, Value_List)), Ctx))
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


check_rebase_rule(Rebase_Rule, Ctx) :-

   (  var(Rebase_Rule)
   -> throw(error(instantiation_error, Ctx))
   ;  Rebase_Rule = '->'(Old_Base, New_Base)
   -> true
   ;  throw(error(type_error((->)/2, Rebase_Rule), Ctx))
   ),
   (  (var(Old_Base); var(New_Base))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_class_arg(Old_Base, Ctx),
   check_class_arg(New_Base, Ctx).


decode_arg(Vals_LOL, Arg_Val, Result, Ctx) :-

   nonvar(Ctx),
   Ctx = context(Pred_Name/Arity, _),
   atom(Pred_Name), integer(Arity),
   
   (  arg_decode(Pred_Name, Arity, Arg_Val, Result0)
   -> true
   ;  (  Vals_LOL = [_|_] % not empty list
      -> (  decode_arg_int(Vals_LOL, Arg_Val, Result0)
         -> assertz(arg_decode(Pred_Name, Arity, Arg_Val,
                               Result0))
         ;  % make the domain and throw the error
            flatten(Vals_LOL, Domain),
            throw(error(domain_error(Domain, Arg_Val), Ctx))
         )
      ;
         throw(error(domain_error(not_empty_list_of_lists,
                                  Vals_LOL)))
      )
   ),
   Result = Result0.

decode_arg_int([], _, _) :- fail.

decode_arg_int([Vals_List|LOL_Tail], Arg_Val, R) :-

   (  gen_memberchk('=@=', Arg_Val, Vals_List)
   -> Vals_List = [R|_] % normalize to the first element
   ;  decode_arg_int(LOL_Tail, Arg_Val, R)
   ).

clear_decode_arg :-

   retractall(arg_decode(_, _, _, _)).
