:- module(ur_debug,
          [assertz_pred/2   % +Key, @Pred
          ]).

:- meta_predicate assertz_pred(+, 0).

assertz_pred(Key, Pred) :-

   assertz(Pred),
   debug(Key, 'assertz(~p)', [Pred]).

