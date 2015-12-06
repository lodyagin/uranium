% Optimized for splitting list representation 

:- module(ur_subarrays,
          [list_subarray/2,     % ?List, ?Subarray
           list_to_subarray/3,  % +List, -Subarray, ?Ctx
           sa_nth0/3,      % ?N, +List, ?Elem
           sa_nth0/4,      % ?N, +List, ?Elem, ?Rest
           sa_nth1/3,      % ?N, +List, ?Elem
           sa_nth1/4,      % ?N, +List, ?Elem, ?Rest
           sa_check/4,     % +Subarray, -Array, -Selection,
                           % +Ctx inst-
           sa_length/2,    % +Subarray, -Length

           sa_select_all/2 % +Subarray, -AllSelected
          ]).

:- use_module(library(clpfd)).
:- use_module(u(ur_intervals)).

%% list_subarray(?List, ?Subarray) is det.
%
% Converts between prolog list and optimized presentation
% for e.g. random_select/6
%
list_subarray(List, s(A, Selection)) :-
   nonvar(List), !,
   A =.. [a|List],
   functor(A, _, N),
   drep_intervals(1..N, Selection).
list_subarray(List, Subarray) :-
   Ctx = context(list_subarray/2, _),
   nonvar(Subarray), !,
   sa_check(Subarray, Array, Selection, Ctx),
   (  Selection == empty
   -> List = []
   ;  findall( X,
               ( intervals_member(K, Selection),
                 arg(K, Array, X) ),
               List)
   ).
list_subarray(_, _) :-
   throw(error(instantiation_error, context(list_subarray/2, _))).

%% list_to_subarray(+List, -Subarray, ?Ctx) is det.
%
% If List is a subarray check it and unifies it with Subarray.
% If List is an array calls list_subarray/2.
%
list_to_subarray(List, Subarray, Ctx) :-
   (  nonvar(List) -> true
   ;  throw(error(instantiation_error, Ctx))
   ),
   (  List = s(_, _)
   -> sa_check(List, _, _, Ctx),
      Subarray = List
   ;  list_subarray(List, Subarray)
   ).


%% sa_check(+Subarray, -Array, -Selection, +Ctx) is det.
%
sa_check(Subarray, Array, Selection, Ctx) :-
   (  Subarray = s(Array, Selection) -> true
   ;   throw(error(type_error(subarray, Subarray), Ctx))
   ),
   (  nonvar(Array) -> true
   ;  throw(error(instantiation_error, Ctx))
   ),
   (  (  compound(Array)
      -> functor(Array, a, N)
      ;  Array == a
      -> N = 0
      ),
      (  clpfd_domain_intervals(Dom, Selection)
      -> true
      ;  throw(error(type_error(subarray, Subarray), Ctx))
      )
   ),
   (  Dom == empty -> true
   ;  clpfd:domain_infimum(Dom, Inf0),
      clpfd:domain_supremum(Dom, Sup0),
      (  Inf0 = n(Inf), Sup0 = n(Sup),
         Inf >= 1, Sup =< N -> true
      ;  throw(error(domain_error(valid_subarray,
                                  s(Array, Selection)),
                     Ctx))
      )
   ).

%% sa_length(+Subarray, -Length)
%
% Gets length of subarray (the length of used interval)
%
sa_length(Subarray, Length) :-
   Ctx = context(sa_length/2, _),
   must_be(nonvar, Subarray),
   sa_check(Subarray, _, Selection, Ctx),
   intervals_size(Selection, Length).

%% sa_nth0(?N, +List, ?Elem)
%
sa_nth0(N, List, Elem) :-
   Ctx = context(sa_nth0/3, _),
   N1 #= N + 1,
   sa_nth1_cmn(N1, List, Elem, _, Ctx).

%% sa_nth0(?N, +List, ?Elem, ?Rest)
%
sa_nth0(N, List, Elem, Rest) :-
   Ctx = context(sa_nth0/4, _),
   N1 #= N + 1,
   sa_nth1_cmn(N1, List, Elem, Rest, Ctx).

%% sa_nth1(?N, +List, ?Elem)
%
sa_nth1(N, List, Elem) :-
   Ctx = context(sa_nth1/3, _),
   sa_nth1_cmn(N, List, Elem, _, Ctx).

%% sa_nth1(?N, +List, ?Elem, ?Rest)
%
sa_nth1(N, List, Elem, Rest) :-
   Ctx = context(sa_nth1/4, _),
   sa_nth1_cmn(N, List, Elem, Rest, Ctx).

%sa_nth1_cmn(_, List, _, _, Ctx) :-
%  var(List), !,
%  throw(error(instantiation_error, Ctx)).
sa_nth1_cmn(N, List, Elem, Rest, Ctx) :-
  var(N), !,
  generate_nth1(N, List, Elem, Rest, Ctx).
sa_nth1_cmn(N, List, Elem, Rest, Ctx) :-
  must_be(positive_integer, N),
  find_nth1(N, List, Elem, Rest, Ctx).

%generate_nth1(N, List, Elem, Rest, Ctx) :-
%%%%  sa_check(Rest, Array, Selected, Ctx), 
%  sa_select_all(Rest, Full),
%
%  functor(Array, _, FullLength),
%  (  FullLength == 0
%  -> N = 1, list_subarray([Elem], List)
%  ;  AllSelected in 1..FullLength,

find_nth1(N, List, Elem, s(Array, Rest), Ctx) :-
  sa_check(List, Array, Selected, Ctx),
  compound(Array),
  dom_nth1(N, Selected, K),
  arg(K, Array, Elem),
  copy_term(Selected, Rest0),
  (  Rest0 #\= K
  -> Rest = Rest0
  ;  Rest = f % special value for empty
  ).



