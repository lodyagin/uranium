:- module(ur_enums,
          [assert_enum/1,             % +List
           basic_enum_field/2,        % ?Field0, ?Field
           enum_integer/2,            % ?Enum, ?Integer
           enum_integer/3,            % +Options, ?Enum, ?Integer
           enum_size/1,               % -Size
           global_cardinality_enum/2  % +Vs, +Pairs
          ]).

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(u(util/lambda)).
:- use_module(u(ur_option)).
:- use_module(u(internal/decode_arg)).

% 'enum_integer#'(Module, Enum, Integer)
:- dynamic 'enum_integer#'/3.

:- meta_predicate assert_enum(:).

%% assert_enum(+List) is det.
%
% Adds new enum definition into enum db.
%
assert_enum(Module:List) :-
   must_be(atom, Module),
   must_be(list, List),
   forall(member(Enum, List), must_be(atom, Enum)),
   retractall('enum_integer#'(Module, _, _)), !,
   forall(nth0(Integer, List, Enum),
          assertz('enum_integer#'(Module, Enum, Integer))).


%% basic_enum_field(?Field0, ?Field)
%
% Basic enum field is that one which ends with '#'.
% Add '#' iff Field0 doesn't contain it.
%
basic_enum_field(Field0, Field) :-
   atom_concat(Field0, '#', Field),
   \+ atom_concat(_, '#', Field0), !.
basic_enum_field(Field, Field).

:- meta_predicate enum_integer(:, :).
:- meta_predicate enum_integer(:, ?, ?).

%% enum_integer(?Enum, ?Integer) is nondet.
%
enum_integer(Module:Enum, Module:Integer) :-
   must_be(atom, Module),
   % check the enum existence
   % NB can't be combined with value retriv. because of
   % clp can cause fail in 'enum_integer#'/3
   (   'enum_integer#'(Module, _, _) -> true
   ;   throw(error(existence_error(enum, Module),
                   context(enum_integer/2, _)))
   ),
   % do the main work
   'enum_integer#'(Module, Enum, Integer).

% get module from options. Set it unbound for global.
extract_module(Options, Module, Policy) :-
   obj_unify(Options, [scope, scope_policy], [Scope, Policy]),
   (  Scope = module(Module)
   -> must_be(atom, Module)
   ;  Scope = field(Field1)
   -> basic_enum_field(Field1, Field),
      options_predicate_to_options_class_name(global:Field, Module)
   ;  true
   ).

%% enum_integer(+Options, ?Enum, ?Integer) is nondet.
%
enum_integer(Options0, Enum, Integer) :-
   Ctx = context(enum_integer/3, _),
   extract_weak(enum_integer, Options0, Options, Weak, Ctx),
   extract_module(Options, Module, ScopePolicy),
   % check the enum existence
   % NB can't be combined with value retrieval because of
   % clp can cause fail in 'enum_integer#'/3
   (   aggregate(count,
                 Module,
                 Module^Enum^Integer^
                   'enum_integer#'(Module, Enum, Integer),
                 NModules)
   ->
       % check the scope policy violation
       (  nonvar(Module) -> true
       ;  ScopePolicy == strict_scope
       -> (  NModules =< 1 -> true
          ;  throw(error(enum_query_error(module_mix_in_strict_policy), Ctx))
          )
       ;  true
       ),
       % do the main work
       'enum_integer#'(Module, Enum, Integer)
   ;
       (  Weak == throw
       -> throw(error(existence_error(enum, Module, Enum, Integer), Ctx))
       ;  Weak == unbound % will fail if Week == fail
       )
   ).

:- meta_predicate enum_size(:).

%% enum_size(-Size) is det.
%
enum_size(Module:Size) :-
   must_be(atom, Module),
   aggregate(count, A^B^'enum_integer#'(Module, A, B), Size).


:- meta_predicate global_cardinality_enum(+, :).

%% global_cardinality_enum(+Vs, +Pairs) is det.
%
% It is the same as global_cardinality/2 but Pairs contains enums as
% keys. Vs are list of enum(Enum, Integer).
% @see enum_integer/2
%
global_cardinality_enum(Vs, M:Pairs) :-
   maplist(\Pair^IPair^( Pair=Key-V, IPair=IKey-V,enum_integer(M:Key, M:IKey), !),
           Pairs, IPairs),
   maplist(\EnumC^Integer^( EnumC=enum(_,Integer) ), Vs, Vs1),
   global_cardinality(Vs1, IPairs),
   maplist(\EnumC^(  EnumC=enum(Enum,Integer),
                     integer(Integer)
                  -> enum_integer(M:Enum,M:Integer), !
                  ;  true ),
           Vs).

:- initialization clear_decode_arg.




