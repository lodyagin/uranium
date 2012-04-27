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
          [reload_all_classes/0
           ]).

:- use_module(u(internal/objects)).
:- use_module(u(internal/objects_i)).
:- use_module(u(internal/class_create)).
:- use_module(u(internal/db_vocab)).
:- use_module(u(internal/ur_debug)).

:- dynamic user:portray/1.

% module_class_def(+Main_Class, -Class) :-
% find all class definitions
module_new_class_def(Main_Class, Class, Parent) :-

   Main_Class:current_predicate(new_class, New_Class_Head),
   functor(New_Class_Head, _, New_Class_Arity),
   (  New_Class_Arity =:= 3
   -> Main_Class:new_class(Class, Parent, _)
   ;  Main_Class:new_class(Class, Parent, _, _)
   ).

all_classes(All_Classes) :-

     % associate each class with a module it is defined
     % add relation to parent classes
     (  current_module(Main_Class),
        u_class(Main_Class),
        module_property(Main_Class, file(Module_File)),
        assertz(objects:module(Main_Class, Module_File)),
        debug(classes,
              'assertz(objects:module(~a, ~a))',
              [Main_Class, Module_File]
             ),
        module_new_class_def(Main_Class, Class, Parent),
        (  %class_id(_, Class)
           objects:clause(module_class_def(Class, _, _), _)
        -> throw(error(class_exists(Class), _))
        ;  true %gen_class_id(Class, Class_Id)
        ),
        assertz(objects:module_class_def(Class, Parent,
                                         Main_Class)),
        debug(classes,
              'assertz(objects:module_class_def(~a, ~a, ~a))',
              [Class, Parent, Main_Class]),
        fail ; true ),

     % TODO check no Main_Class repeats
     % TODO check no class name repeats

     % represet parent relations as a graph edges
     findall(Parent - Class,
             objects:module_class_def(Class, Parent, _),
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
          'objects:assertz(copy(~d, ~a, ~p, ~p) :- ~a:~p',
          [Class_Id, Class, Copy_From, Copy_To, Module, Term]),
    fail ; true ),

    % assert reinterpret/4
   (call(Module:current_predicate(reinterpret, Term)),
    Term = reinterpret(Class_From, Class_To, Obj_From, Obj_To),
    call(Module:clause(Term, _)),
    objects:assertz(
     reinterpret(Class_From, Class_To, Obj_From, Obj_To) :-
                   Module:Term
                   ),
    debug(classes,
          'objects:assertz(reinterpret(~a, ~a, ~p, ~p) :- ~a:~p',
          [Class_From, Class_To, Obj_From, Obj_To, Module, Term]),
    fail ; true ),

    % process class module-scoped objects
   (  Class == Module
   -> % import downcast/4 predicates
      dynamic_import(Module, objects, downcast),
                                % process typedefs
      process_typedefs(Module)
   ;  true ),

   class_create_cmn(Class, Parent, Add_Fields, Key, _, Ctx).


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
     (  memberchk(pretty_print - TD_PP_Head, TD_List)
     -> TD_PP_Pred =.. [TD_PP_Head, TD_Stream, TD_Value, TD_Opt],
        objects:assertz(pretty_print(TD_Type, TD_Stream, TD_Value, TD_Opt)
                        :- Module:TD_PP_Pred),
        debug(classes,
              'objects:assertz(pretty_print(~p, ~p, ~p, ~p) :- ~a:~p',
              [TD_Type, TD_Stream, TD_Value, TD_Opt, Module,
               TD_PP_Pred])
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

     fail
  ;
     true
  ).


%
% import all predicates with head Functor as dynamic assert
%
dynamic_import(Module_From, Module_To, Functor) :-

  Module_From:current_predicate(Functor, Term),
  Module_From:clause(Term, _),
  Module_To:assertz(Term :- Module_From:Term),
  debug(classes,
        '~a:assertz(~p :- ~a:~p)',
        [Module_To, Term, Module_From, Term]),
  fail
  ;
  true.

reload_all_classes :-

   db_vocab_clear(_), % clear all current db class caches
   
   % clear the db
   retractall_objects,
   retractall(db_pg:pl_pg_type(_, _, _)),

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

   % Get classes in the creation order

   all_classes(Class_Defs),
   % + module/2
   % + module_class_def/3

   (  member(Class_Def, Class_Defs),
      
      process_class_def(Class_Def),
      % + class_id/3
      % + eval_field/5
      % + copy/4        :- ...
      % + reinterpret/4 :- ...
      % + downcast/4    :- ...
      % + typedef_flag/2
      % + pretty_print/4 :- ...
      % + db_pg:pl_pg_type/3
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

   install_v_portrays. % TODO it should be defined in _v files

install_v_portrays :-

   (  clause(portray(X), object_module:_) -> true
   ;  user:assertz(portray(X) :- object_module:v_portray(X))
   ).

% v_portray(Obj) :-

%    u_object(Obj), !,
%    obj_pretty_print([], Obj).

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
             (  file_search_path(u, Dir)
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
     ).

:- initialization reload_all_classes.

