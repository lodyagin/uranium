:- module(html_piece_v, []).

:- use_module(u(v)).
:- use_module(u(html/v/standard/html_tag_v)).
:- use_module(u(logging)).


new_class(html_piece_v, http_result_v,
          [dom,
          node_path, node_rpath, xpath,
           root_node % toplevel html_piece_v or a
                     % descendant. Leave unbound for
                     % toplevel object to prevent cyclic
                     % term.
           ]
          %[www_address, xpath]
          % xpath must be from the root_node
         ).

'html_piece_v?'(Obj, class, Class) :-

   obj_field(Obj, dom, DOM),
   (  ground(DOM),
      functor(DOM, element, 3)
   -> Class = html_tag_v
   ;  functor(Obj, Class, _)  % leave unchanged
   ).

% TODO return the real version
'html_piece_v?'(_, html_version, html_4_01_transitional).

downcast(html_piece_v, html_tag_v, From, To) :-

   obj_field(From, dom, DOM),
   ground(DOM),
   DOM = element(Tag, Attrs, _),
   obj_field(To, html_tag, Tag),
   unify_html_attrs(To, Attrs, [], Bulk),
   obj_field(To, '.@bulk', Bulk).

%typedef(dom, [pretty_print - hide_dom]).

%hide_dom(_, _, Options) :-

%   write_log('<DOM>', Options).

