:- module(site_scc, [site_scc/2]).

:- use_module(library(uri)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(algorithm/tarjan)).
:- use_module(u(action/click_url)).
:- use_module(u(parser/html/html_page_parse)).

site_scc(Url, Scc) :-

   uri_normalized(Url, Url1),

   load_page(Url1, _, _, Start_Page),

   db_clear(pages), db_clear(links),
   
   tarjan(pages,
          page_v, www_address / http_request_url,
          load_page, resolve_links(links),
          Start_Page,
          _,
          Scc).

load_page(Url, _, _, Page) :-
   click_url(Url, Page0),
   obj_rebase((object_v -> tarjan_vertex_v), Page0, Page).

resolve_links(DB, Page, Link_Urls) :-
   html_page_parse(DB, Page, [html_local_link_v]),
   Page_Url ^= Page / www_address / http_request_url,
   findall(Link_Url,
           db_select(DB, 
                     [functor, http_request_url, link_url],
                     [html_local_link_v, Page_Url, Link_Url]),
           Link_Urls).



   