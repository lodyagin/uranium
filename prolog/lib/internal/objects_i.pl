:- module(objects_i,
          [
           class_fields/3,
           class_id/2,
           class_primary_id/2,
           field_names_list/2,
           fields_names_types/3,
           gen_class_id/2,
           get_key/2,
           obj_class_id/2,
           u_class/1,
           u_object/1
           ]).

:- use_module(library(lists)).

:- multifile prolog:message/3.

:- dynamic objects:arity/2,    % Class_Id, Arity
           objects:class_id/3, % Class_Id, Id_Primary, Class
           objects:copy/3,
           objects:downcast/4,
           objects:field/6,
           objects:fields/2,   % Class_Id, Field_List
           objects:key/2,
           objects:module/2,
           objects:module_class_def/3, % Class, Parent, Module
           objects:parent/2, % Class_Id, Parent_Class_Id
           objects:pretty_print/4,
           objects:typedef_flag/2.

% class_fields(+Class_Id, ?Native, ?Fields)
% Get list of fields/types 
% Native = true means no field from parent
% (Native = false - only parent fields)
  
class_fields(Class_Id, Native, Fields) :-

   findall(Field_Name:Field_Type,
      objects:clause(
         field(Class_Id, Field_Name, _, _, Field_Type, Native),
         _ ),
      Fields
  ).
  
class_id(Class_Id, Class) :-

   nonvar(Class_Id), !,
   objects:class_id(Class_Id, _, Class), !. % ensure no BT

class_id(Class_Id, Class) :-

   objects:class_id(Class_Id, _, Class).


class_primary_id(Class, Class_Id) :-

   objects:class_id(Class_Id, true, Class), !.


field_names_list(Class_Id, Fields) :-

   objects:fields(Class_Id, Fields), !.


% parse Name : Type lists and check all values
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


% TODO not thread safe
gen_class_id(Class, Class_Id) :-

   nonvar(Class), var(Class_Id),
   (  class_id(Class_Id, Class)
   -> throw(class_exists(Class))
   ;  true ),
     
   findall(Id, class_id(Id, _), Ids),
   max_list(Ids, Class_Id1),
   Class_Id is Class_Id1 + 1.
                             

%
% get_key(+Class_Id, -Key)
%

get_key(Class_Id, Key) :-

  (objects:key(Class_Id, Key) -> true ; Key = []).


obj_class_id(Object, Class_Id) :-

   arg(1, Object, Class_Id).


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


prolog:message(class_system_bad_state(Details)) -->
                             
   ['The Uranium class system may be corrupted: '],
   [ nl ],
   [Details].

prolog:message(class_exists(Class)) -->
                             
   ['The class ~a is defined already' - [Class]].

prolog:message(invalid_object(Object, Details)) -->

   ['Invalid object passed: ~w ' - [Object]],
   [ '(', Details, ')' ].

prolog:message(no_object_field(Object, Field_Name)) -->

   ['There is no such field `~a'' in the object ~w'
    - [Field_Name, Object]].
                             

