:- module(html_tag_html_v, []).

:- use_module(u(ixpath)).


new_class(html_tag_html_v, html_tag_v, []).

'html_tag_html_v?'(Obj, class, Class) :-

   (  ixpath(/body, Obj, _)
   -> Class = page_v
   ;  true ).


downcast(html_tag_html_v, page_v, From, To) :-

   (  ixpath(/head/title, From, Title)
   -> obj_field(To, title, Title)
   ;  true ).