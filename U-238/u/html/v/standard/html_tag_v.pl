:- module(html_tag_v, []).

:- use_module(u(v)).

new_class(html_tag_v, html_piece_v, [html_tag, html_tag_attrs]).

'html_tag_v?'(Obj, class, Class) :-

   obj_field(Obj, html_tag, Tag),
   (  ground(Tag),
      concat_atom([html_tag_, Tag, '_v'], '', Class),
      class_exists(Class)
   -> true
   ;  functor(Obj, Class, _) % leave unchanged
   ).
   