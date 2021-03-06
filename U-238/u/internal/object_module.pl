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

:- module(object_module,
          [assert_clause_int/3,         % Module_From:Clause, +Module_To,
                                        % :Checking_Pred

           check_downcast_impl/1,       % +Clause
	   load_class_module/1,            % +FileName
           reload_all_classes/0,
	   unload_class_module/1           % +FileName
           ]).

:- use_module(library(error)).
:- use_module(u(internal/class_create)).
:- use_module(u(internal/db_vocab)).
:- use_module(u(internal/objects)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/ur_debug)).
:- use_module(u(ur_lists)).
:- use_module(u(util/lambda)).

:- dynamic user:portray/1, user:file_search_path/2.

% module_class_def(+Main_Class, -Class) :-
% find all class definitions
module_new_class_def(Main_Class, Class, Parent) :-

   Main_Class:current_predicate(new_class, New_Class_Head),
   functor(New_Class_Head, _, New_Class_Arity),
   (  New_Class_Arity =:= 3
   -> Main_Class:new_class(Class, Parent, _)
   ;  Main_Class:new_class(Class, Parent, _, _)
   ).

% assert_class_modules/0 is det.
% Assert objects:module(Main_Class, Module_File_Path) for each
% class module.
assert_class_modules :-
   (  current_module(Main_Class),
      u_class(Main_Class),
      module_property(Main_Class, file(Module_File)),
      assertz_pred(classes,
                   objects:module(Main_Class, Module_File)),
      fail ; true
   ).

assert_all_module_class_defs(Main_Class) :-
   forall(
     (  module_new_class_def(Main_Class, Class, Parent),
        (  objects:clause(module_class_def(Class, _, _), _)
        -> throw(error(class_exists(Class), _))
        ;  true
        )),
      assertz_pred(classes,
                   objects:module_class_def(Class, Parent,
                                            Main_Class))
   ).

% all_classes(-All_Classes) is det.
% All_Classes will contain class names in the order to create
all_classes(All_Classes) :-

     % associate each class with a module it is defined
     % add relation to parent classes
     (  objects:module(Main_Class, _),
        assert_all_module_class_defs(Main_Class),
        fail ; true
     ),

     % TODO check no Main_Class repeats
     % TODO check no class name repeats

     all_classes_by_module(_, All_Classes). % all classes
     
all_classes_by_module(MainClass, All_Classes) :-
     % represet parent relations as a graph edges
     findall(Parent - Class,
             objects:module_class_def(Class, Parent, MainClass),
             Edges),

     vertices_edges_to_ugraph([], Edges, Graph),
     (	 top_sort(Graph, Classes)
     ->	 true
     ;	 throw(class_inheritance_cycle(Graph))
     ),
     findall(new_class(Class_X, Parent_X, Add_Fields_X, Key_X),
             (member(Class_X, Classes),
              objects:module_class_def(Class_X, _, Main_Class),
              nonvar(Main_Class),
	      (  Main_Class:clause(new_class(Class_X, Parent_X,
                                             Add_Fields_X),
                                   _),
                 Key_X = _
              ;  Main_Class:clause(new_class(Class_X, Parent_X,
                                             Add_Fields_X, Key_X),
                                   _)
              )
             ),
             All_Classes
            ).


% Class module-wide definitions
process_module_def(Module) :-
   % reinterpret
   dynamic_import(Module, objects, reinterpret, 4, true),
   % typedef
   process_typedefs(Module),
   % downcast
   dynamic_import(Module, objects, downcast, 4,
                  check_downcast_impl).


process_class_def(new_class(Class, Parent, Add_Fields, Key)) :-

   Ctx = context(process_class_def/1, _),

   % Get the definition module
   objects:module_class_def(Class, Parent, Module), !,

   % Generate new Class_Id
   gen_class_id(Class, Class_Id),
   assertz(objects:class_id(Class_Id, true, Class)),
   debug(classes,
         'assertz(objects:class_id(~d, true, ~a))',
         [Class_Id, Class]),

   % assert *_v? predicates first (they are used by others)
   (atom_concat(Class, '?', Head),
    call(Module:current_predicate(Head, Term)),
    call(Module:clause(Term, _)),
    Term =.. [Head, E_Obj, E_Field, E_Value],

    (   atom(E_Field) -> true
    ;   print_message(error, domain_error(arg2_is_atom, Term)),
        fail
    ),

    % can assert several eval_field/5 for each Class
    objects:assertz(eval_field(Class, E_Field, E_Obj, E_Value,
                               Module:Term)),
    debug(classes, '~p',
          [objects:assertz(eval_field(Class, E_Field, E_Obj,
                                      E_Value, Module:Term))]),
    fail ; true ),

    % assert copy/4
   (call(Module:current_predicate(copy, Term)),
    Term = copy(Copy_Class, Copy_From, Copy_To),
    call(Module:clause(Term, _)),
    (  u_class(Copy_Class)
    -> Copy_Class = Class
    ;  print_message(error,
                     class_definition_error(Module, Term)),
       fail
    ),
    objects:assertz(
                    copy(Class_Id, Class, Copy_From, Copy_To) :-
                   Module:Term
                   ),
    debug(classes,
          'objects:assertz(copy(~d, ~a, ~p, ~p) :- ~a:~p)',
          [Class_Id, Class, Copy_From, Copy_To, Module, Term]),
    fail ; true
   ),
   class_create_cmn(Class, Parent, Add_Fields, Key, _, Ctx).


%% check_downcast_impl(:Clause) is det.
% Check downcast/4 definitions
:- meta_predicate check_downcast_impl(0).

check_downcast_impl(Module:(Term :- _)) :-
   Term = downcast(From_Class, To_Class, _, _),
   (  \+ (u_class(From_Class), class_primary_id(From_Class, _))
   -> print_message(warning,
                    bad_downcast_impl(Module:Term,
    'From_Class must be bounded to already defined uranium classes'))
   ;  \+ (u_class(To_Class), class_primary_id(To_Class, _))
   -> print_message(warning,
                    bad_downcast_impl(Module:Term,
    'To_Class must be bounded to already defined uranium classes'))
   ;  true
   ).

process_typedefs(Module) :-

  (  call(Module:current_predicate(typedef/2)),
     call(Module:typedef(TD_Type, TD_List)),

     % control for not repeating type definitions
     (  objects:typedef_flag(TD_Type, Some_Class)
     -> print_message(warning,
		      type_redefined(TD_Type, Some_Class)),
        !, fail
     ;  assertz(objects:typedef_flag(TD_Type, Module)),
        debug(classes,
              'objects:typedef_flag(~p, ~a)',
              [TD_Type, Module])
     ),

     % pretty_print
     (  memberchk(pretty_print - TD_PP_Head0, TD_List)
     -> ( TD_PP_Head0 = TD_PP_Module:TD_PP_Head -> true
        ; TD_PP_Head0 = TD_PP_Head, TD_PP_Module = Module
        ),
        TD_PP_Head =.. TD_PP_Head_List,
        append(TD_PP_Head_List, 
               [TD_Stream, TD_Value, TD_Opt], TD_PP_Pred_List),
        TD_PP_Pred =.. TD_PP_Pred_List,
        objects:assertz(pretty_print(TD_Type, TD_Stream, TD_Value, TD_Opt)
                        :- TD_PP_Module:TD_PP_Pred),
        debug(classes,
              'objects:assertz(pretty_print(~p, ~p, ~p, ~p) :- ~a:~p',
              [TD_Type, TD_Stream, TD_Value, TD_Opt, TD_PP_Module,
               TD_PP_Pred])
     ;  true
     ),

     % value_set
     (  memberchk(value_set - VS_Head0, TD_List)
     -> ( VS_Head0 = ','(VS_Module:VS_Head, VS_Opts) -> true
        ; VS_Head0 = ','(VS_Head, VS_Opts) -> VS_Module = Module
        ; VS_Head0 = VS_Module:VS_Head -> true
        ; VS_Head0 = VS_Head, VS_Module = Module
        ),
        VS_Head =.. VS_Head_List,
        append(VS_Head_List, [VS_Opt0, VS_Opt, VS_Value0, VS_Value], 
               VS_Pred_List),
        VS_Pred =.. VS_Pred_List,
        Pred = (value_set(TD_Type, VS_Opt0, VS_Opt, VS_Value0, VS_Value):- 
                VS_Module:VS_Pred),
        objects:assertz(Pred),
        debug(classes, 'objects:assertz(~p)', [Pred]),
        (  var(VS_Opts) -> true
        ;  functor(VS_Pred, VS_Pred_Functor, _),
           assertz_pred(classes, 
                        objects:value_options(TD_Type, 
                                              VS_Module:VS_Pred_Functor, 
                                              Module:VS_Opts))
        )
     ;  true
     ),

     % postgres types
     (  memberchk(postgres - type(PG_Type, PG_Convert_Pred),
                  TD_List)

     -> db_pg:assertz(pl_pg_type(TD_Type, PG_Type,
                                      Module:PG_Convert_Pred)
                          )
     ; true
     ),

     % oracle types
     (  memberchk(plsql - arg(PLSQL_Type, PLSQL_InOut), TD_List)
     -> assertz(plsql_argument(TD_Type, PLSQL_Type, PLSQL_InOut))
     ;  true
     ),     

     fail
  ;
     true
  ).


% dynamic_import(+Module_From, +Module_To, +Functor, +Arity, :Checking_Pred)
% is det.
% Import all predicates with head Functor as dynamic assert
% with additional checking defined by Checking_Pred(:Clause)

:- meta_predicate dynamic_import(+, +, +, +, 1).

dynamic_import(Module_From, Module_To, Functor, Arity, Checking_Pred) :-

   (  functor(Term, Functor, Arity),
      Module_From:clause(Term, _),
      assert_clause_int(Module_From:(Term :- Term), Module_To, Checking_Pred),
      fail
   ;  true
   ).

% it's a trick to fight a warning
true(_).


%% assert_clause_int(:Clause, +Module_To, :Checking_Pred) is semidet.
%
% Assert a new Clause in a Module if Checking_Pred(:Clause) succeeds.
% Checking_Pred can be just `true'.

:- meta_predicate assert_clause_int(:, +, 1).

assert_clause_int(Module_From:Clause, Module_To, Checking_Pred) :-

   must_be('/'(':-',2), Clause),
   Clause = (Term :- Body),
   must_be(compound, Term),
   must_be(compound, Body),
   (  ( Checking_Pred = _:true ; Checking_Pred == true )
   -> true
   ;  call(Checking_Pred, Module_From:Clause)
   ),
   assertz_pred(classes, Module_To:(Term :- Module_From:Body)).

normalize_file_name(FileName0, FileName) :-
    absolute_file_name(FileName0, FileName1),
    (   atom_concat(_, '.pl', FileName1)
    ->  FileName = FileName1
    ;   atom_concat(FileName1, '.pl', FileName)
    ).

%% load_class_module(+FileName) is det.
% Load class(es) definition file.
% The file can have non standard name and be in non standard directory.
load_class_module(FileName) :-
    Ctx = context(load_class_def/2, _),
    must_be(ground, FileName),
    catch(
	    load_class_module_int(FileName, Ctx),
	    Ex,
	    (  unload_class_module(FileName), % rollback
	       throw(Ex) )
	).

    
load_class_module_int(FileName, Ctx) :-
    normalize_file_name(FileName, FileName2),
    consult(FileName2),
    module_property(MainClass, file(FileName2)), !, %NB MainClass must be the module name
    (   objects:module(MainClass, _)
     -> throw(error(class_exists(MainClass), Ctx))
     ;  true
    ),
    assertz_pred(classes, objects:module(MainClass, FileName2)),
    % load all classes from the file
    assert_all_module_class_defs(MainClass),
    all_classes_by_module(MainClass, ModuleClasses),
    forall( member(Class, ModuleClasses), process_class_def(Class) ),
    process_module_def(MainClass).
    
%% unload_class_module(+FileName) is det.
unload_class_module(FileName) :-
    normalize_file_name(FileName, FileName2),
    module_property(MainClass, file(FileName2)), !,
    must_be(atom, MainClass), % for sure
    all_classes_by_module(MainClass, ModuleClasses),
    forall(
	    ( member(new_class(Class, _, _, _), ModuleClasses), class_id(ClassId, Class) ),
	    retract_object(ClassId, Class) % fixme no typedefs cleared
	),
    forall(
	    module_new_class_def(MainClass, Class, _),
	    retractall(objects:module_class_def(Class, _, _))
	),
    retractall(objects:module(MainClass, _)),
    unload_file(FileName2).
    
%% reload_all_classes is det.
% Reload all class definitions.
reload_all_classes :-

   ch_vocab_clear,    % clear class hierarchy
   db_vocab_clear(_), % clear all current db class caches
   
   % clear the db
   retractall_objects,
   retractall(db_pg:pl_pg_type(_, _, _)),
   retractall(object_module:plsql_argument(_, _, _)),

   % Assert definitions for object_base_v
   objects:assertz(arity(0, 1)),
   debug(classes, 'objects:assertz(arity(0, 0))', []),
   objects:assertz(class_id(0, true, object_base_v)),
   debug(classes, 'objects:assertz(class_id(0, true, object_base_v))', []),
   objects:assertz(parent_(0, -1)),
   debug(classes, 'objects:assertz(parent_(0, -1))', []),
   objects:assertz(typedef_flag(hidden, object_base_v)),
   debug(classes, 'objects:assertz(typedef_flag(hidden, object_base_v))', []),
   objects:assertz(key(0, 0, [])),
   debug(classes, 'objects:assertz(key(0, 0, []))', []),
   objects:assertz(rebased_class(object_base_v, [], 0)),
   debug(classes, 'objects:assertz(rebased_class(object_base_v, [], 0))', []),

   assertz_pred(classes, objects:next_class_id(1)),

   % Load all class modules
   (  find_class_module(Module_Path),
      consult(Module_Path),
      fail ; true
   ),

   assert_class_modules,
   % + module/2

   % Get classes in the creation order
   all_classes(Class_Defs),
   % + module_class_def/3

   (  member(Class_Def, Class_Defs),

      process_class_def(Class_Def),
      % + class_id/3
      % + eval_field/5
      % + copy/4        :- ...
      %
      % >> class_create:
      % + parent_/2
      %
      % >>> assert_new_class_id
      % >>>> assert_class_fields:
      % + field/4       :- ...
      % + field_info/5
      % <<<<
      % + arity/2
      % + rebased_class/3
      % <<<
      % + key/3
      % + copy/4 :- ... (for descendants)
      % <<

      fail ; true
   ),

   (  objects:module(Module_Name, _),
      process_module_def(Module_Name),
      % + reinterpret/4 :- ...
      % + typedef_flag/2
      % + pretty_print/4 :- ...
      % + value_set/5 :- ...
      % + value_options/3
      % + db_pg:pl_pg_type/3
      % + downcast/4    :- ...

      fail ; true
   ),

   install_v_portrays. % TODO it should be defined in _v files

install_v_portrays :-

   (  clause(portray(_), object_module:_) -> true
   ;  user:assertz(portray(X) :- object_module:v_portray(X))
   ).

v_portray(Object) :-
   compound(Object), u_object(Object), !,
   obj_class_id(Object, Class_Id),
   integer(Class_Id),
   functor(Object, Class, _),
   class_id(Class_Id, Class),
   format('~a(~d, ', [Class, Class_Id]), 
   findall(Field-Value,
           (  objects:field_info(Class_Id, Field, _, _, false),
              obj_field_int(Class_Id, Field, _, Object, Value, _, _)
           ), List),
   write_delimited(\FV^(F-V=FV -> format('~a:~p', [F, V]); write(FV)),
                   ', ', List),
   write(')').
   
v_portray(El) :-
   compound(El), functor(El, element, _), !,
   \+ dom_level(El, 0, 4), % Hide DOMs after 4 level
   write('<element/3>').

dom_level(L, _, Max_Level) :-

   is_list(L), !,
   length(L, N),
   N =< Max_Level.

dom_level(element(_, _, L), N, Max_Level) :- !,

   N =< Max_Level,
   N1 is N + 1,
   dom_level(L, N1, Max_Level).

dom_level(_, _, _).

class_module_file(Start, Path) :-

   Start \== '',
   (  atom_concat(Path_Prefix, '/', Start) -> true
   ;  Path_Prefix = Start ),

   between(0, 5, Level),
   (  bagof('*', Cnt^between(1, Level, Cnt), Asterisks)
   -> concat_atom([Path_Prefix|Asterisks], '/', Dir)
   ;  Dir = Path_Prefix ),

   concat_atom([Dir, '*_v.pl'], '/', Path).

%
% find_class_module(-Module_Path)
%
% Return all class modules pathes on BT
% (up to 5th level)
%

find_class_module(Module_Path) :-

  findall(Files,
          (
             (  ( user:file_search_path(U, Dir),
                  atom(Dir),
                  \+ memberchk(U, [app_preferences, autoload,
                                   foreign, library, path, pce, swi,
                                   user_profile])
                )
             ;   working_directory(Dir, Dir)
             ),
             class_module_file(Dir, Mask),
             expand_file_name(Mask, Files)
          ),
          List1
  ),
  flatten(List1, List),
  member(Module_Path, List),
  \+ ( sub_atom(Module_Path, _, _, _, 'hide/')
     ; sub_atom(Module_Path, _, _, _, 'HIDE/')
     ),

  add_objects_dir(Module_Path).

add_objects_dir(Path) :-

   file_directory_name(Path, Dir),
   (  user:file_search_path(v, Dir) -> true
   ;  assertz(user:file_search_path(v, Dir))
   ).

:- initialization reload_all_classes.

