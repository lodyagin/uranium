:- module(site_scc, [site_scc/2]).

:- use_module(library(uri)).
:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(algorithm/tarjan)).
:- use_module(u(action/click_url)).
:- use_module(u(parser/html/html_page_parse)).

site_scc(Url, Scc) :-

   uri_normalized(Url, Url1),

   load_page(Url1, Start_Page),

   db_clear(pages), db_clear(links),
   
   tarjan(pages,
          page_v, http_request_url,
          load_page, resolve_links(links),
          Start_Page,
          Scc).

load_page(Url, Page) :-

   click_url(Url, Page0),
   obj_rebase((object_v -> tarjan_vertex_v), Page0, Page).

resolve_links(DB, Page, Link_Urls) :-

   html_page_parse(DB, Page, [local_link_v]),
   obj_field(Page, http_request_url, Page_Url),
   findall(Link_Url,
           db_select(DB, 
                     [functor, http_request_url, link_url],
                     [local_link_v, Page_Url, Link_Url]),
           Link_Urls).



   