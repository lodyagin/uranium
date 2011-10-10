:- module(html_piece_v, []).

:- use_module(u(html/v/standard/html_tag_v)).

                     
new_class(html_piece_v, http_result_v, [dom, xpath]).

'html_piece_v?'(Obj, class, Class) :-

   obj_field(Obj, dom, DOM),
   (  ground(DOM),
      functor(DOM, element, 3)
   -> Class = html_tag_v
   ;  functor(Obj, Class, _)  % leave unchanged
   ).

downcast(html_piece_v, html_tag_v, From, To) :-

   obj_field(From, dom, DOM),
   ground(DOM),
   DOM = element(Tag, Attrs, _),
   obj_field(To, html_tag, Tag),
   unify_html_attrs(To, Attrs, [], Bulk),
   obj_field(To, '.@bulk', Bulk).
