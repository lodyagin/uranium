:- module(site_scc, [site_scc/2]).

:- use_module(u(algorithm/tarjan)).
:- use_module(u(ur_url)).
:- use_module(u(action/click_url)).
:- use_module(u(parser/html/html_page_parse)).

site_scc(Url, Scc) :-

   url_normalize(Url, Url1),

   load_page(Url1, Start_Page),

   db_clear(site_scc),
   tarjan(site_scc,
          page_v, http_request_url,
          load_page, resolve_links,
          Start_Page,
          Scc).

load_page(Url, Page) :-

   click_url(Url, Page0),
   obj_rebase((object_v -> tarjan_vertex_v), Page0, Page).

resolve_links(Page, Link_Urls) :-

   DB = 'site_scc#links_tmp',
   db_clear(DB),
   html_page_parse(DB, Page, [local_link_v]),
   db_select_list(DB, local_link_v, [link_url], Links_Urls0),
   flatten(Links_Urls0, Link_Urls).



   