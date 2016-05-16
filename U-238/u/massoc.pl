% It is a multimap implementation.

:- module(massoc,
          [del_massoc/4, %+Key, +MAssoc0, ?Value, -MAssoc
           get_massoc/3, %+Key, +MAssoc, -Value
           get_massoc/5, %+Key, +MAssoc0, ?Val, -MAssoc, +NewVal
           min_massoc/3, %+MAssoc, -Key, -Value
           put_massoc/4  %+Key, +MAssoc0, +Value, -MAssoc
          ]).

:- use_module(library(assoc)).

%% get_massoc(+Key, +MAssoc0, ?Val, -MAssoc, +NewVal) is nondet.
%
% Can leave a choicepoint, use it with once/1 if only one solution
% needed.
get_massoc(Key, MAssoc0, Val, MAssoc, NewVal) :-
   get_assoc(Key, MAssoc0, List0, MAssoc, [NewVal|List]),
   select(Val, List0, List).

%% get_massoc(+Key, +MAssoc, -Value) is nondet.
get_massoc(Key, MAssoc, Value) :-
   get_assoc(Key, MAssoc, List),
   member(Value, List).

%% put_massoc(+Key, +MAssoc0, +Value, -MAssoc) is det.
put_massoc(Key, MAssoc0, Value, MAssoc) :-
   (  get_assoc(Key, MAssoc0, List, MAssoc, [Value|List])
   -> true
   ;  put_assoc(Key, MAssoc0, [Value], MAssoc)
   ).

%% del_massoc(+Key, +MAssoc0, ?Value, -MAssoc) is nondet.
%
% Can leave a choicepoint, use it with once/1 if only one solution
% needed.
del_massoc(Key, MAssoc0, Value, MAssoc) :-
   get_assoc(Key, MAssoc0, List0, MAssoc1, List),
   (  List0 = [Value]
   -> del_assoc(Key, MAssoc0, _, MAssoc)
   ;  select(Value, List0, List),
      MAssoc1 = MAssoc
   ).

%% min_massoc(+MAssoc, -Key, -Value) is nondet.
%
% Returns all values for the minimal key.
min_massoc(MAssoc, Key, Value) :-
   min_assoc(MAssoc, Key, Values),
   member(Value, Values).