:- module(html_attrs_v,
          [
           construct_html_attrs/2,
           unify_html_attrs/4
           ]).

new_class(html_attrs_v, object_v, ['@id', '@class', '@@bulk']).

unify_html_attrs(_, [], Rest, Rest) :- !. 
unify_html_attrs(Obj, [Id = Val|T], Rest0, Rest) :-
   atom_concat('@', Id, Field),
   (  obj_field_wf(Obj, Field, Val)
   -> Rest1 = Rest0
   ;  Rest1 = [Id = Val|Rest0] ),
   unify_html_attrs(Obj, T, Rest1, Rest).

construct_html_attrs(Attr_List, Obj) :-

   obj_construct(html_attrs_v, [], [], Obj),
   unify_html_attrs(Obj, Attr_List, [], Rest),
   obj_field(Obj, '@@bulk', Rest).