:- module(html_tag_v,
          [
           unify_html_attrs/4
           ]).

:- use_module(u(v)).

new_class(html_tag_v, html_piece_v,
          [html_tag,
          '.id',
           '.class',
           '.title',
           '.style',
           '.dir',
           '.xml:lang',
           '.@bulk']).

'html_tag_v?'(Obj, class, Class) :-

   obj_field(Obj, html_tag, Tag),
   (  ground(Tag),
      concat_atom([html_tag_, Tag, '_v'], '', Class),
      class_exists(Class)
   -> true
   ;  functor(Obj, Class, _) % leave unchanged
   ).

downcast(html_tag_v, html_tag_input_v, From, To) :-

   obj_field(From, '.@bulk', Old_Bulk),
   unify_html_attrs(To, Old_Bulk, [], New_Bulk),
   obj_field(To, '.@bulk', New_Bulk).

unify_html_attrs(_, [], Rest, Rest) :- !. 
unify_html_attrs(Obj, [Id = Val|T], Rest0, Rest) :-
   atom_concat('.', Id, Field),
   (  obj_field_wf(Obj, Field, Val)
   -> Rest1 = Rest0
   ;  Rest1 = [Id = Val|Rest0] ),
   unify_html_attrs(Obj, T, Rest1, Rest).

