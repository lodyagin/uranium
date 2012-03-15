:- module(decode_arg,
          [
           decode_arg/4,  % +Vals_LOL, +Arg_Val, -Result, +Ctx
           clear_decode_arg/0
          ]).

:- dynamic arg_decode/4.

% decode_arg(+Vals_LOL, +Arg_Val, -Result, +Ctx)
%
% Unify Result with first element of the Vals_LOL sublist
% containing Arg_Val.
%
% <NB> All modules used this pred must call
% clear_decode_arg in the initialization section.

decode_arg(Vals_LOL, Arg_Val, Result, Ctx) :-

   nonvar(Ctx),
   Ctx = context(Pred_Name/Arity, _),
   atom(Pred_Name), integer(Arity),

   (  nonvar(Arg_Val)
   -> Arg_Val1 = Arg_Val
   ;  Arg_Val1 = f(_)
   % will not match with any arg but only with _
   ),
   
   (  arg_decode(Pred_Name, Arity, Arg_Val1, Result0)
   -> true
   ;  (  Vals_LOL = [_|_] % not empty list
      -> (  decode_arg_int(Vals_LOL, Arg_Val, Result0)
         -> 
            assertz(arg_decode(Pred_Name, Arity, Arg_Val1,
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
