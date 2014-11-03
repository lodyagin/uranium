% -*- fill-column: 65; -*-
%
% This file is a part of Uranium, a general-purpose functional
% test platform.
%
% Copyright (C) 2011, Sergei Lodyagin
% Copyright (C) 2012, Kogorta OOO Ltd
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later
% version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General
% Public License along with this library; if not, write to the
% Free Software Foundation, Inc., 51 Franklin Street, Fifth
% Floor, Boston, MA 02110-1301 USA
%
% e-mail: lodyagin@gmail.com
% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(check_arg,
          [
           check_class_arg/2,          % +Class, Ctx (inst-)
           check_db_key/2,             % inc. inst (inst+)
           check_existing_class_arg/2, % +Class, Ctx (inst-)
           check_existing_class_arg/3, % +Class, Ctx, -Class_Id
                                       % inst-

           check_existing_class_list_arg/3, % +Class_List, +Ctx,
                                            % -Class_Id_List inst+

           check_field_name/2,         % inst+
           check_fields_arg/2,         % not inc. inst (inst-)
           check_inst/2,
           check_list_fast_arg/2,
           check_values_arg/3, % +Field_List, +Value_List, +Ctx
                               % inst-

           check_values_partlist_arg/3, % +Field_List,
                                        % +Value_List, +Ctx
                                        % inst-

           check_object_arg/3,         % +Obj, +Ctx, -Class_Id inst-
           check_rebase_rule/4,  % +Rule, +Ctx, -Old_Base, -New_Base
                                 % -inst+

           error:has_type/2
           ]).

:- reexport(u(internal/db_i), [db_key_is_valid/1]).

/** <module> Check arguments

  It is like library(error) of SWI-Prolog but also shows context.
*/

:- use_module(library(error)).
:- use_module(objects_i).
%:- use_module(db_i).

:- multifile error:has_type/2.

error:has_type(Functor/Arity, X) :-

  atom(Functor),
  %has_type(nonneg, Arity),
  functor(X, Functor, Arity).

%% check_inst(@Arg, @Ctx) is det.
%
% Check whether Arg is instantiated.
%
% @error instantiation_error

check_inst(Arg, Ctx) :-

   (  var(Arg)
   -> throw(error(instantiation_error, Ctx))
   ;  true
   ).


%% check_class_arg(+Class, Ctx) is semidet.
%
% Check whether Class is valid Uranium class name. Doesn't
% include the instantiation check (it will fail if Class is a
% free variable) and check of Class existence. It uses u_class/1.
%
% @error type_error(atom, Class)
% @error domain_error(uranium_class, Class)

check_class_arg(Class, Err_Context) :-

   nonvar(Class),
   (  \+ atom(Class)
   -> throw(error(type_error(atom, Class), Err_Context))
   ;  u_class(Class)
   -> true
   ;  throw(error(domain_error(uranium_class, Class),Err_Context))
   ).


%% check_existing_class_arg(+Class, @Ctx) is semidet.
%
% It performs check_class_arg/2 and then checks Class is a name
% of some existing class.
%
% @error type_error(atom, Class)
% @error domain_error(uranium_class, Class)
% @error existence_error(uranium_class, Class)

check_existing_class_arg(Class, Ctx) :-

   check_existing_class_arg(Class, Ctx, _).

%% check_existing_class_arg(+Class, @Ctx, -Class_Id) is semidet.
%
% It is the same as check_existing_class_arg/2 but returns
% Class_Id for Class.
%
% @error type_error(atom, Class)
% @error domain_error(uranium_class, Class)
% @error existence_error(uranium_class, Class)

check_existing_class_arg(Class, Ctx, Class_Id) :-

   check_class_arg(Class, Ctx),
   (  class_primary_id(Class, Class_Id) -> true
   ;  throw(error(existence_error(uranium_class, Class), Ctx))
   ).

check_existing_class_list_arg(Class_List, Ctx, Class_Id_List) :-

   check_inst(Class_List, Ctx),
   check_existing_class_list_arg2(Class_List, Ctx,
                                  Class_Id_List).

check_existing_class_list_arg2([], _, []) :- !.

check_existing_class_list_arg2([Class|CT], Ctx, [Class_Id|CIT]) :-

   check_existing_class_arg(Class, Ctx, Class_Id),
   check_existing_class_list_arg2(CT, Ctx, CIT).


%% check_db_key(?DB_Key, +Ctx)
% Check valid DB_Key. Also check whether it is instantiated.
check_db_key(DB_Key, Ctx) :-

   check_inst(DB_Key, Ctx),
   (  db_key_is_valid(DB_Key)
   -> true
   ;  throw(error(domain_error(db_key, DB_Key), Ctx))
   ).


%% check_fields_arg(+Field_Names, @Ctx) is semidet.
%
% Check whether Field_Names is a proper (not partial) list of
% Uranium object field names. Each field name is checked with
% check_field_name/2. The predicate fails if Field_Names is a
% free variable (must be checked with check_inst/2 before).
%
% @error instantiation_error - in the case of a partial list or
% variable presence in the list
% @error type_error(atom, Field_Name)

check_fields_arg(Field_Names, Ctx) :-

   nonvar(Field_Names),
   check_field_names(Field_Names, Field_Names, Ctx).

check_field_names([], _, _) :- !.

check_field_names([Field_Name|T], Full, Ctx) :-

   check_field_name(Field_Name, Ctx),
   (  nonvar(T) -> true
   ;  throw(error(instantiation_error, Ctx))
   ),
   check_field_names(T, Full, Ctx), !.

check_field_names(_, Full, Ctx) :-

   throw(error(type_error(list, Full), Ctx)).

%% check_field_name(@Field_Name, @Ctx) is det.
%
% Check whether Field_Name is a valid field name of Uranium
% object (it should be instantiated and be an atom).
%
% @error instantiation_error
% @error type_error(atom, Field_Name)

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

%% check_values_arg(+Field_List, @Value_List, @Ctx) is semidet.
%
% Check value list for predicates which accept Field_List with
% matched Value_List. Check whether Value_List is a proper (not
% partial) list of the same length.
% It fails if Field_List or Value_List is a free variable.
%
% @error type_error(list, Value_List)
% @error domain_error(matched_list_length, (Field_List,
% Value_List))
%
% @see check_values_partlist_arg/3

check_values_arg(Field_List, Value_List, Ctx) :-

   nonvar(Field_List), nonvar(Value_List),
   ( \+ is_list(Value_List)
   -> throw(error(type_error(list, Value_List), Ctx))
   ;  length(Field_List, LL), length(Value_List, LL)
   -> true
   ;  throw(error(domain_error(matched_list_length,
                  (Field_List, Value_List)), Ctx))
   ).

check_values_partlist_arg(Field_List, Value_List, Ctx) :-

   nonvar(Value_List),
   ( \+ is_of_type(list_or_partial_list, Value_List)
   -> throw(error(type_error(list, Value_List), Ctx))
   ;  length(Field_List, LL), length(Value_List, LL)
   -> true
   ;  throw(error(domain_error(matched_list_length,
                  (Field_List, Value_List)), Ctx))
   ).

%% check_object_arg(+Object, @Ctx, -Class_Id) is semidet.
%
% Check whether Object is Uranium object. Doesn't include the
% instantiation check (it will fail if Object is a free
% variable). It uses u_object/1.
%
% @param Class_Id class ID of Object
% @error type_error(uranium_object, Object)
% @error invalid_object(Object, 'invalid class id')

check_object_arg(Object, Err_Context, Class_Id) :-

   nonvar(Object),
   (  \+ u_object(Object)
   -> throw(error(type_error(uranium_object, Object), Err_Context))
   ;  obj_class_id(Object, Class_Id),
      integer(Class_Id),
      functor(Object, Class, _),
      class_id(Class_Id, Class)
   -> true
   ;  throw(invalid_object(Object, 'invalid class id'))
   ).


check_rebase_rule(Rebase_Rule, Ctx, Old_Base, New_Base) :-

   (  var(Rebase_Rule)
   -> throw(error(instantiation_error, Ctx))
   ;  Rebase_Rule = '->'(Old_Base, New_Base)
   -> true
   ;  throw(error(type_error((->)/2, Rebase_Rule), Ctx))
   ),
   (  (var(Old_Base); var(New_Base))
   -> throw(error(instantiation_error, Ctx))
   ;  true ),
   check_existing_class_arg(Old_Base, Ctx),
   check_existing_class_arg(New_Base, Ctx).


