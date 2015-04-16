:- module(ur_enums,
          [assert_enum/1,             % +List
           enum_integer/2,            % ?Enum, ?Integer
           enum_size/1,               % -Size
           global_cardinality_enum/2  % +Vs, +Pairs
          ]).

:- use_module(library(aggregate)).
:- use_module(library(error)).

% enum_integer(Module, Enum, Integer)
:- dynamic enum_integer/3.

:- meta_predicate assert_enum(:).

%% assert_enum(+List) is det.
%
% Adds new enum definition into enum db.
%
assert_enum(Module:List) :-
   must_be(atom, Module),
   must_be(list, List),
   forall(member(Enum, List), must_be(atom, Enum)),
   retractall(enum_integer(Module, _, _)), !,
   forall(nth0(Integer, List, Enum), assertz(enum_integer(Module, Enum, Integer))).

:- meta_predicate enum_integer(:, :).

%% enum_integer(?Enum, ?Integer) is nondet.
%
enum_integer(Module:Enum, Module:Integer) :- 
   must_be(atom, Module),
   enum_integer(Module, Enum, Integer).

:- meta_predicate enum_size(:).

%% enum_size(-Size) is det.
%
enum_size(Module:Size) :-
   must_be(atom, Module),
   aggregate(count, A^B^enum_integer(Module, A, B), Size).


%% global_cardinality_enum(+Vs, +Pairs) is det.
%
% It is the same as global_cardinality/2 but for enums instead of
% integers.
%
%global_cardinality_enum(Vs, Pairs) :-

