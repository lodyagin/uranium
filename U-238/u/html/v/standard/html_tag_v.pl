:- module(html_tag_v,
          [
           gen_html_tag_downcast/2,
           unify_html_attrs/4
           ]).

:- use_module(u(v)).

new_class(html_tag_v, html_piece_v,
          [html_tag,
           '.accesskey',
           '.class',
           '.dir',
           '.id',
           '.lang',
           '.onclick',
           '.ondblclick',
           '.onkeydown',
           '.onkeypress',
           '.onkeyup',
           '.onmousedown',
           '.onmousemove',
           '.onmouseout',
           '.onmouseover',
           '.onmouseup',
           '.style',
           '.tabindex',
           '.title',
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
   gen_html_tag_downcast(From, To).

downcast(html_tag_v, html_tag_label_v, From, To) :-
   gen_html_tag_downcast(From, To).

unify_html_attrs(_, [], Rest, Rest) :- !. 
unify_html_attrs(Obj, [Id = Val|T], Rest0, Rest) :-
   atom_concat('.', Id, Field),
   (  obj_field(Obj, fail, Field, Val)
   -> Rest1 = Rest0
   ;  Rest1 = [Id = Val|Rest0] ),
   unify_html_attrs(Obj, T, Rest1, Rest).

gen_html_tag_downcast(From, To) :-

   obj_field(From, '.@bulk', Old_Bulk),
   unify_html_attrs(To, Old_Bulk, [], New_Bulk),
   obj_field(To, '.@bulk', New_Bulk).

