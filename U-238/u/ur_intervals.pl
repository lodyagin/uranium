% Operating on intervals.
/*
   An intervals in Uranium are represented as trees. Each node is one of:

   empty: empty interval.

   split(N, Left, Right, Size)
      - split on integer N, with Left and Right domains whose elements are
        all less than and greater than N, respectively. The domain is the
        union of Left and Right, i.e., N is a hole. Size is a size
        of the interval.

   from_to(From, To)
      - interval (From-1, To+1); From and To are bounds
*/
% @author Sergei Lodyagin

:- module(ur_intervals,
          [any_to_intervals/2, % +Any, -Intervals
           clpfd_domain_intervals/2, % ?Dom, ?Intervals
           drep_intervals/2, % ?Drep, ?Intervals
           intervals_member/2, % ?El, +Dom
           intervals_nth0/3, % ?Idx, +Dom, ?N
           intervals_nth0/4, % ?Idx, +Dom, ?N, ?Rest
           intervals_nth1/3,  % ?Idx, +Dom, ?N
           intervals_nth1/4,  % ?Idx, +Dom, ?N, ?Rest
           intervals_size/2  % +Interval, -Size
          ]).

:- use_module(library(error)).
:- use_module(library(clpfd)).

%% intervals_size(+Intervals, -Size) is det.
%
% Size is a number of elements in Intervals.
%
intervals_size(Intervals, _) :-
  var(Intervals), !,
  instantiation_error(Intervals).
intervals_size(split(_, _, _, Size), Size) :- !.
intervals_size(empty, 0) :- !.
intervals_size(from_to(From, To), Size) :- !,
  Size is To - From + 1,
  Size > 0.
intervals_size(Term, _) :-
  domain_error(uranium_intervals, Term).

%% intervals_member(?El, +Interval) is nondet.
%
intervals_member_int(Y, from_to(From, To)) :- !,
  between(From, To, Y).
intervals_member_int(Y, split(_, Left, Right, _)) :-
  var(Y), !,
  (  intervals_member_int(Y, Left)
  ;  intervals_member_int(Y, Right)
  ).
intervals_member_int(Y, split(N, Left, Right, _)) :-
  nonvar(Y),
  (   Y < N
  ->  intervals_member_int(Y, Left)
  ;   intervals_member_int(Y, Right)
  ).

intervals_member(El, Interval) :-
  must_be(ground, Interval),
  intervals_member_int(El, Interval).

%% intervals_nth0(?Idx, +Interval, ?N)
%
intervals_nth0(Idx, Interval, N) :-
  Ctx = context(intervals_nth0/3, _),
  intervals_nth0_cmn(Idx, Interval, N, Ctx).

%% intervals_nth0(?Idx, +Interval, ?N, ?Rest)
%
intervals_nth0(Idx, Interval, N, Rest) :-
  Ctx = context(intervals_nth0/4, _),
  intervals_nth0_cmn(Idx, Interval, N, Rest, Ctx).

%% intervals_nth1(?Idx, +Interval, ?N)
%
intervals_nth1(Idx, Interval, N) :-
  Ctx = context(intervals_nth1/3, _),
  Idx0 #= Idx - 1,
  intervals_nth0_cmn(Idx0, Interval, N, Ctx).

%% intervals_nth1(?Idx, +Interval, ?N, ?Rest)
%
intervals_nth1(Idx, Interval, N, Rest) :-
  Ctx = context(intervals_nth1/4, _),
  Idx0 #= Idx - 1,
  intervals_nth0_cmn(Idx0, Interval, N, Rest, Ctx).

intervals_nth0_cmn(Idx, _, _, Ctx) :-
  var(Idx), !,
  throw(error(not_implemented, Ctx)).
intervals_nth0_cmn(Idx, Intervals, N, Ctx) :-
  (  has_type(nonneg, Idx)
  -> intervals_nth0_int(Idx, Intervals, N)
  ;  throw(error(domain_error(nonneg, Idx), Ctx))
  ).
intervals_nth0_cmn(Idx, _, _, _, Ctx) :-
  var(Idx), !,
  throw(error(not_implemented, Ctx)).
intervals_nth0_cmn(Idx, Intervals, N, Rest, Ctx) :-
  (  has_type(nonneg, Idx)
  -> intervals_nth0_int(Idx, Intervals, N, Rest)
  ;  throw(error(domain_error(nonneg, Idx), Ctx))
  ).

intervals_nth0_int(Idx, from_to(From, To), Value) :- !,
  Value is From + Idx,
  Value =< To.
intervals_nth0_int(Idx, split(_, Left, Right, Size), Value) :- !,
  intervals_size(Left, LeftSize),
  (  Idx < LeftSize
  -> intervals_nth0_int(Idx, Left, Value)
  ;  Idx < Size
  -> Idx1 is Idx - LeftSize,
     intervals_nth0_int(Idx1, Right, Value)
  ).
intervals_nth0_int(_, empty, _) :- !, fail.
intervals_nth0_int(_, I, _) :- domain_error(intervals, I).
intervals_nth0_int(Idx, from_to(From, To), Value, Rest) :- !,
   (  Idx == 0
   -> Value = From,
      succ(From, From1),
      (  From1 =< To -> Rest = from_to(From1, To) ; Rest = empty )
   ;  MaxIdx is To - From,
      (  Idx == MaxIdx
      -> Value = To,
         succ(To1, To),
         (  To1 >= From -> Rest = from_to(From, To1) ; Rest = empty )
      ;  Idx < MaxIdx,
         Value is From + Idx,
         succ(To1, Value), succ(Value, From2),
         Rest = split(Value, from_to(From, To1), from_to(From2, To),
                      MaxIdx)
      )
   ).
intervals_nth0_int(Idx, split(Hole, Left, Right, Size), Value, Rest) :-
  !,
  succ(Size1, Size),
  intervals_size(Left, LeftSize),
  (  Idx < LeftSize
  -> intervals_nth0_int(Idx, Left, Value, RestLeft),
     (  RestLeft == empty
     -> Rest = Right
     ;  Rest = split(Hole, RestLeft, Right, Size1)
     )
  ;  Idx < Size
  -> Idx1 is Idx - LeftSize,
     intervals_nth0_int(Idx1, Right, Value, RestRight),
     (  RestRight == empty
     -> Rest = Left
     ;  Rest = split(Hole, Left, RestRight, Size1)
     )
  ).
intervals_nth0_int(_, empty, _, _) :- !, fail.
intervals_nth0_int(_, I, _, _) :- domain_error(intervals, I).

%% any_to_clpfd_domain(?Any, -Domain) is det.
%
% Converts different types to clpfd domain
%
any_to_clpfd_domain(Any, Domain) :-
  (  var(Any)
  -> (  fd_var(Any)
     -> clpfd:fd_get(Any, Domain, _)
     ;  instantiation_error(Any)
     )
  ;  clpfd:is_drep(Any)
  -> clpfd:drep_to_domain(Any, Domain)
  ;  domain_error(convertible_to_clpfd_domain, Any)
  ).

% drep_intervals(?Drep, ?Intervals) is det.
%
drep_intervals(Drep, Intervals) :-
   nonvar(Drep), !,
   clpfd:drep_to_domain(Drep, Dom),
   clpfd_domain_intervals(Dom, Intervals).
drep_intervals(Drep, Intervals) :-
   nonvar(Intervals), !,
   clpfd_domain_intervals(Dom, Intervals),
   clpfd:domain_to_drep(Dom, Drep).

any_to_intervals(Any, Intervals) :-
   any_to_clpfd_domain(Any, Dom),
   clpfd_domain_intervals(Dom, Intervals).

%% cplfd_domain_intervals(?Dom, ?Intervals) is det.
%
% Converts between clpfd domain and an uranium interval.
clpfd_domain_intervals(Dom, Intervals) :-
   clpfd_domain_intervals_int(Dom, Intervals, _).

clpfd_domain_intervals_int(D, I, _) :- var(D), var(I), !,
   instantiation_error(_).
clpfd_domain_intervals_int(empty, empty, 0) :- !.
clpfd_domain_intervals_int(from_to(n(F), n(T)), from_to(F, T), S) :- !,
   S is T - F + 1,
   S > 0.
clpfd_domain_intervals_int(D, split(N, L, R, Size), Size) :-
   D = split(N, L1, R1), !,
   clpfd_domain_intervals_int(L1, L, S1),
   clpfd_domain_intervals_int(R1, R, S2),
   Size is S1 + S2.
clpfd_domain_intervals_int(D, I, _) :-
   (  var(D)
   -> domain_error(intervals, I)
   ;  domain_error(finite_clfd_domain, D)
   ).