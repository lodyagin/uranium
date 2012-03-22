%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011  Sergei Lodyagin
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(objects_i,
          [
           class_all_fields/2,  % +Class_Id, -Fields
                                % nonevals only
           class_arity/2,

           % @Pattern, +Class_Id, ?Native, ?Eval, ?Fields
           class_fields/5,

           class_id/2,
           class_new_fields/2,  % +Class_Id, -Fields
           class_primary_id/2,
           common_parent/3,
           fields_names_types/3,
           gen_class_id/2,
           gen_new_class_id/1,
           get_key/2,
           get_keymaster/2,
           is_rebased_class/1,
           list_inheritance/2,
           list_inheritance/3,
           obj_class_id/2,
           obj_construct_int/5,
           obj_field_int/7,
           obj_reset_fields_int/6,
           obj_rewrite_int/7,   % +Class_Id, +Object0, +Fields,
                                % ?Old_Vals, +New_Vals, -Object,
                                % +Ctx

           obj_unify_int/6,     % +Class_Id, +Fields, +Weak,
                                % +Term, ?Value, +Ctx

           parent/2,            % ?Id, ?Parent_Id
           same_or_descendant/3,%+Parent_Id, +No_Rebased, ?Desc_Id
           u_class/1,
           u_object/1,
           unbounded_fields/2, % +Obj, -Field_Names
           prolog:message/3
           ]).

:- use_module(u(internal/objects)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(u(ur_lists)).
:- use_module(u(internal/decode_arg)).

:- multifile prolog:message/3.

class_all_fields(Class_Id, Fields) :-

   class_fields(_, Class_Id, _, false, Fields).


% class_arity(+Class_Id, -Arity)

class_arity(Class_Id, Arity) :-

   integer(Class_Id),
   (  objects:arity(Class_Id, Arity) -> true
   ;  throw(class_system_bad_state(
            'no objects:arity/2 for class id ~d' - Class_Id))
   ).


% class_fields(@Pattern, +Class_Id, ?Native, ?Eval, ?Fields)
% Get list of fields/types
% Native == true means no field from parent
% Native == false - only parent fields
% Native unbound - both
%
% Eval == true - only eval fields
% Eval == false - only non-eval fields
% Eval unbound - both
%
% It returns ordset (coz it uses setof)

class_fields(Pattern, Class_Id, Native, Eval, Fields) :-

   var(Pattern), !,

   class_fields(_-_, Class_Id, Native, Eval, Fields0),
   pairs_keys(Fields0, Fields).

class_fields(_-_, Class_Id, Native, Eval, Fields) :- !,

   class_fields2(Class_Id, Native, Eval, Fields).

class_fields(Pattern, Class_Id, Native, Eval, Fields) :-

   functor(Pattern, Symbol, 2),
   class_fields(_-_, Class_Id, Native, Eval, Fields0),
   pairs_replace_functor(Symbol, Fields0, Fields).

class_fields2(Class_Id, Native, Eval, Fields) :-

   (  setof(Field_Name-Field_Type,
        Native^Eval^(objects:field_info(Class_Id, Field_Name,
                                        Field_Type, Native, Eval)
                    ),
        Fields0
      )
   -> predsort(compare_obj_fields, Fields0, Fields)
   ;  Fields = []
   ).

compare_obj_fields(Delta, E1-_, E2-_) :-

   compare(Delta, E1, E2).

class_id(Class_Id, Class) :-

   nonvar(Class_Id), !,
   objects:class_id(Class_Id, _, Class), !. % ensure no BT

% Nondet!
% Use it only for get all Class_Ids (primary + rebased)
% In the case you need a single id see class_primary_id
class_id(Class_Id, Class) :-

   objects:class_id(Class_Id, _, Class).


class_new_fields(Class_Id, Fields) :-

   class_fields(_, Class_Id, true, false, Fields).

% det
class_primary_id(Class, Class_Id) :-

   objects:class_id(Class_Id, true, Class), !.


% common_parent(+Class1_Id, +Class2_Id, -Cmn_Parent_Id)
%
% find the common parent's id

common_parent(Class1_Id, Class2_Id, Cmn_Parent_Id) :-

   list_inheritance(Class1_Id, List1),
   list_inheritance(Class2_Id, List2),
   common_head_rev(List1, List2, [Cmn_Parent_Id|_]).

%
% list_inheritance(+Class_Id, -List)
%
% represent inheritance as [class_id|...]
% e.g. list_inheritance(1, [0, 1]).

list_inheritance(Class_Id, List) :-

   list_inheritance(0, Class_Id, [], List).


list_inheritance(From_Id, To_Id, List) :-

   list_inheritance(From_Id, To_Id, [], List).


list_inheritance(Id, Id, List, [Id|List]) :- !.

list_inheritance(From_Id, To_Id, List0, List) :-

   objects:parent_(To_Id, Parent_Id),
   list_inheritance(From_Id, Parent_Id, [To_Id|List0], List).


% fields_names_types(?Fields_Def, ?Names, ?Types)

fields_names_types([], [], []).

fields_names_types([Name:Type|FT], [Name|NT], [Type|TT]) :-

   nonvar(Type), !,
   (  var(Name) -> throw(error(instantiation_error, _))
   ; true ),
   (  \+ atom(Name) -> throw(error(type_error(atom, Name), _))
   ; true ),
   (  \+ atom(Type) -> throw(error(type_error(atom, Type), _))
   ; true ),
   fields_names_types(FT, NT, TT).

fields_names_types([Name|FT], [Name|NT], [_|TT]) :-

   (  var(Name) -> throw(error(instantiation_error, _))
   ; true ),
   (  \+ atom(Name) -> throw(error(type_error(atom, Name), _))
   ; true ),
   fields_names_types(FT, NT, TT).


gen_class_id(Class, Class_Id) :-

   nonvar(Class), var(Class_Id),
   (  class_id(Class_Id, Class)
   -> throw(class_exists(Class))
   ;  true ),

   gen_new_class_id(Class_Id).


gen_new_class_id(Class_Id) :-

   retract(next_class_id(Class_Id)),
   succ(Class_Id, New_Id),
   assertz(next_class_id(New_Id)).


%
% get_key(+Class_Id, -Key)
%

get_key(Class_Id, Key) :-

  nonvar(Class_Id),
  (objects:key(Class_Id, _, Key) -> true ; Key = []).


%
% get_keymaster(+Class_Id, -Keymaster_Id)
%

get_keymaster(Class_Id, Keymaster_Id) :-

  objects:key(Class_Id, Keymaster_Id, _).


is_rebased_class(Class_Id) :-

   nonvar(Class_Id),
   objects:class_id(Class_Id, false, _).


obj_class_id(Object, Class_Id) :-

   arg(1, Object, Class_Id).


parent(Id, Parent_Id) :-

   objects:parent_(Id, Parent_Id), !,
   Id =\= 0.

obj_construct_int(Class_Id, Field_Names, Weak, Field_Values,
                  Object) :-

   Ctx = context(obj_construct_int/5, _),
   % FIXME move this checking to callers
   decode_arg([[throw, throws, strict, s],
               [unbound, weak, w],
               [fail, false, f]
              ], Weak, Weak1, Ctx),

   class_id(Class_Id, Class),
   class_arity(Class_Id, Arity),
   functor(Object, Class, Arity),
   arg(1, Object, Class_Id),
   obj_unify_int(Class_Id, Field_Names, Weak1, Object,
                 Field_Values, Ctx).

% obj_field_int(Class_Id, Field_Name, Weak, Obj, Value, Type, Ctx)
% :-

%    % whether to be det or nondet
%    (   nonvar(Field_Name) -> Det = t ; Det = f ),
%    (   objects:field_info(Class_Id, Field_Name, Type, _, Is_Eval),
%        (  Is_Eval == true
%        -> once(objects:field(Class_Id, Field_Name, Obj, Value))
%        ;  objects:field(Class_Id, Field_Name, Obj, Value)
%        )
%    *-> (Det = t -> ! ; true)
%    ;   obj_field_int_error(Class_Id, Weak, Field_Name, Det, Ctx)
%    ).

obj_field_int(Class_Id, Field_Name, Weak, Obj, Value, Type, Ctx) :-

   (   nonvar(Field_Name) -> Det = t ; Det = f ),

   (   objects:field_info(Class_Id, Field_Name, Type, _, Is_Eval)
   *->
       (   Is_Eval == true
       ->  once(objects:field(Class_Id, Field_Name, Obj, Value))
       ;   objects:field(Class_Id, Field_Name, Obj, Value)
       )
   ;
       obj_field_int_error(Weak, Field_Name, Obj, Ctx)
   ),
   (   Det = t -> ! ; true ).


% the version for eval/noneval fields only
% Field_Name must be checked with
% objects:field_info(Class_Id, Field_Name, _, _, Eval)
% before the call
obj_field_int_part(false, Class_Id, Field_Name, Obj, Value) :-

   objects:field(Class_Id, Field_Name, Obj, Value).

obj_field_int_part(true, Class_Id, Field_Name, Obj, Value) :-

   once(objects:field(Class_Id, Field_Name, Obj, Value1)),
   Value = Value1.



obj_field_int_error(Weak, Field_Name, Obj, Ctx) :-

   (  Weak = unbound
   -> true %% return an unbound value
   ;  Weak = fail
   -> fail
   ;  Weak = throw
   -> throw(error(no_object_field(Obj, Field_Name), Ctx))
   ;  Self_Ctx = context(obj_field_int_error/4, _),
      decode_arg([[unbound], [fail], [throw]], Weak, _,
                 Self_Ctx) % report the error
   ).


obj_reset_fields_int(Class_Id, Fields_List, Object0, Object,
                     Weak, Ctx) :-

   list_to_ord_set(Fields_List, Reset_Set),
   class_all_fields(Class_Id, All_Fields_Set),

   % check the 'strict' condition
   (  Weak = strict
   -> % must not be nonexisting fields
      ord_subtract(Reset_Set, All_Fields_Set, [])
   ;  true ),

   ord_subtract(All_Fields_Set, Reset_Set, Copy_Set),

   obj_unify_int(Class_Id, Copy_Set, Weak, Object0, Values, Ctx),
   obj_construct_int(Class_Id, Copy_Set, Weak, Values, Object).

% obj_rewrite_int(+Class_Id, +Object0, +Fields, ?Old_Vals,
%                 +New_Vals, -Object, +Ctx)

obj_rewrite_int(Class_Id, Object0, Fields, Old_Vals, New_Vals,
                Object, Ctx) :-

   obj_unify_int(Class_Id, Fields, throw, Object0, Old_Vals, Ctx),
   obj_reset_fields_int(Class_Id, Fields, Object0, Object, throw,
                        Ctx),
   obj_unify_int(Class_Id, Fields, throw, Object, New_Vals, Ctx).


% obj_unify_int(+Class_Id, +Fields, +Weak, +Term, ?Values, +Ctx)

obj_unify_int(Class_Id, Fields, Weak, Term, Values, Ctx) :-

   nonvar(Fields), % it protects from wrong results

   % unify noneval first (they can change result for evals)
   obj_unify_int(Fields, [], Fields1, Class_Id, false,
                 Term, Values, [], Values1),

   % Fields1 are the fields not found by the prev call
   obj_unify_int(Fields1, [], Fields2, Class_Id, true,
                 Term, Values1, [], _),

   (   Fields2 = [] -> true % all fields are used
   ;   [Error_Field|_] = Fields2, % for the first field only
                                  % (actual for throw case)
       obj_field_int_error(Weak, Error_Field, Term, Ctx)
   ).

obj_unify_int([], AF, AF, _, _, _, [], VF, VF).

obj_unify_int([Field|FT], AF0, AF, Class_Id, Eval, Term,
              [Value|VT], VF0, VF) :-

   obj_unify_one(Field, AF0, AF1, Class_Id, Eval, Term,
                 Value, VF0, VF1),
   obj_unify_int(FT, AF1, AF, Class_Id, Eval, Term, VT, VF1, VF).

obj_unify_one(Field, AF0, AF, Class_Id, Eval, Term, Value,
              VF0, VF) :-

   (   nonvar(Field) -> Det = t ; Det = f ),

   (   objects:field_info(Class_Id, Field, _, _, Eval)
   *-> obj_field_int_part(Eval, Class_Id, Field, Term, Value),
       AF = AF0, VF = VF0
   ;   AF = [Field|AF0], VF = [Value|VF0]
   ),

   (   Det = t -> ! ; true ).


% same_or_descendant(+Parent_Id, +No_Rebased, ?Desc_Id)

same_or_descendant(Id, No_Rebased, Id) :-

   (  No_Rebased == true
   -> \+ is_rebased_class(Id)
   ;  true ).

same_or_descendant(Parent_Id, No_Rebased, Desc_Id) :-

   Parent_Id \== Desc_Id,
   (  No_Rebased == true
   -> \+ is_rebased_class(Parent_Id)
   ;  true ),
   objects:parent_(Id, Parent_Id),
   same_or_descendant(Id, No_Rebased, Desc_Id).


%% u_class(@Class)
%  True if Class is a valid name for an uranium class

u_class(Class) :-

   atom(Class),
   atom_concat(_, '_v', Class).


%% u_object(@Term)
%  True if Term is bound to an uranium object.

u_object(Term) :-

   compound(Term),
   functor(Term, Functor, _), %NB arity can be 0 (object_v)
   atom_concat(_, '_v', Functor).

% unbounded_fields(+Obj, -Field_Names)
%
% Return all names of unbound (var) noneval fields in Obj
% Attention: Field_Names is not ordered, just a simple list

unbounded_fields(Obj, Field_Names) :-

   obj_class_id(Obj, Class_Id),
   findall(Field,
           (objects:field_info(Class_Id, Field, _, _, false),
            objects:field(Class_Id, Field, Obj, Value),
            var(Value)
           ),
           Field_Names
          ).

% Error messages for the Uranium object system

prolog:message(class_system_bad_state(Details)) -->

   ['The Uranium class system may be corrupted: '],
   [ nl ],
   [Details].

prolog:message(implementation_error(Format, Args)) -->

   ['Internal Uranium error: ', nl],
   [Format - Args].

prolog:message(class_exists(Class)) -->

   ['The class ~a is defined already' - [Class]].

prolog:message(class_inheritance_cycle(Graph)) -->

   ['There is a cycle in class inheritance: ~w'
    - Graph].

prolog:message(type_redefined(Type, Orig_Class)) -->

   ['Type ~a is already defined in class ~a'
    - [Type, Orig_Class]],
   [nl, 'The new definition was ignored.'].

prolog:message(invalid_object(Object, Details)) -->

   ['Invalid object passed: ~p ' - [Object]],
   [ '(', Details, ')' ].

prolog:message(no_object_field(Object, Field_Name)) -->

   ['There is no such field `~a'' in the object ~p'
    - [Field_Name, Object]].

prolog:message(undef_operation(Op_Name, Class_Id)) -->

   ['The operation `~a'' is not defined for class id ~d'
   - [Op_Name, Class_Id]].

prolog:message(bad_eval_result(Object, Field)) -->

   ['User-defined field `~a'' evaluation failed for ~p'
   - [Field, Object]].

prolog:message(bad_downcast_impl(Mode, Object, Class_From,
                                 Class_To, Result)) -->

   ['User-defined ~a implementation is bad:' - [Mode]],
   [ nl ],
   ['~a -> ~a transforms ~p to ~p'
   - [Class_From, Class_To, Object, Result]].

prolog:message(not_downcast(From_Class, To_Class)) -->

   ['~a -> ~a is not downcast' - [From_Class, To_Class]].


:- initialization clear_decode_arg.
