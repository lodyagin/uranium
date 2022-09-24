% -*- fill-column: 65; -*- 

:- module(html_tag_li_v_parse,
          [html_tag_li_v_parse/3
%           html_tag_li_v_cast/3
           ]).

/** <module> Parse html_tag_li
  */

:- use_module(u(ur_atoms)).
:- use_module(u(ur_terms)).
:- use_module(u(ur_lists)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(logging)).
:- use_module(u(internal/check_arg)).
:- use_module(library(xpath)).
:- use_module(library(lists)).

:- use_module(u(internal/check_arg)).

html_tag_li_v_parse(Object0, Class, Object) :-
	Ctx = context(html_tag_li_v_parse/3, _),
	check_existing_class_arg(Class, Ctx),
	obj_field(Object0, dom, Data),
	obj_construct(Class, [], [], Object),
	foreach(
					(xpath(Data, //meta, element(meta, L, _)),
					 memberchk(itemprop=Field0, L),
					 memberchk(content=Value, L),
					 trim_atom(both, [32], Field0, Field)
					),
					(
					 obj_field(Object, fail, Field, Value) -> true
					; write_log(['Unkown ', Class, ' field: [', Field, ']'])
					)
				 ).

% html_tag_li_v_cast(DB_Key, List_Item, Class_To) :-

%    Ctx = context(html_tag_li_v_cast/7, _),
%    check_db_key(DB_Key, Ctx),
%    check_inst(List_Item, Ctx),
%    check_inst(Class_To, Ctx),
%    check_object_arg(List_Item, Ctx, _),
%    check_existing_class_arg(Class_To, Ctx),
   
% 	 html_tag_li_v_parse(List_Item, Class_To, Object),
% 	 db_put_object(DB_Key, Object, _).
