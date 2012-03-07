% Lambda functions

:- module(lambda,
	  [arg_reorder/4,
	   arg_reorder/5,
           lambda/3,
           lambda/5
	  ]).

:- use_module(u(ur_lists)).

:- meta_predicate arg_reorder(2, +, ?, ?).
:- meta_predicate arg_reorder(3, +, ?, ?, ?).
:- meta_predicate lambda(+, 0, +).
:- meta_predicate lambda(+, +, 0, +, +).

% Argument reordering

arg_reorder(Goal, Order, Arg1, Arg2) :-

   extract_by_key_order(Order, [1 - Arg1, 2 - Arg2], L1),
   pairs_values(L1, Reordered_Args),
   apply(Goal, Reordered_Args).

arg_reorder(Goal, Order, Arg1, Arg2, Arg3) :-

   extract_by_key_order(Order,
                        [1-Arg1, 2-Arg2, 3-Arg3],
                        L1),
   pairs_values(L1, Reordered_Args),
   apply(Goal, Reordered_Args).

% Lambda adapters

lambda(X1, Pred, X) :-

   catch(ignore((X1 = X,
                 (  call(Pred) -> true
                 ;  throw(fail)
                 ),
                 fail
                )),
         _,
         fail).
         
lambda(X1, Y1, Pred, X, Y) :-

   catch(ignore((X1 = X, Y1 = Y,
                 (  call(Pred) -> true
                 ;  throw(fail)
                 ),
                 fail
                )),
         _,
         fail).
         


