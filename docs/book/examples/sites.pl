:- module(sites,
          [site/1, 
           page_link/2, page_links/2,
           top_level/1, top_level/2,
           domain_site/2
           ]).

%:- use_module(library(http/http_client)).
%:- use_module(library(http/http_sgml_plugin)).
:- use_module(html/http_page).
:- use_module(html/http_ops).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(ur_objects)).

:- dynamic top_level/1, site/1.

site('classicfm.co.uk').
%site('gismeteo.ua').
%site('sourceforge.net').
%site('kogorta.dp.ua').

top_level('.com').
top_level('.net').
top_level('.org').
top_level('.info').
top_level('.co.uk').
top_level('.dp.ua').

top_level(Domain, Top_Domain) :-

  concat_atom(Parts_R, '.', Domain),
  reverse(Parts_R, [First, Second|_]),
  (  atom_length(First, First_Len), First_Len > 2
  -> Top = [First]
  ;  atom_length(Second, Second_Len), Second_Len =< 3
  -> Top = [First, Second]
  ;  Top = [First]
  ),
  reverse(Top, Top_R),
  concat_atom([''|Top_R], '.', Top_Domain).

add_new_site(Site) :-

  Pred = site(Site),
  (  recorded(sites, Pred)
  -> format("\n*~a", Site)
  ;  format("\n+~a", Site),
     recordz(sites, Pred)
  ).

go :-

   ( site(X),
     \+ recorded(visited_sites, site(X)),
     add_new_site(X), fail
   ; true
   ), 
  
   recorded(sites, site(Site)),
   \+ recorded(visited_sites, site(Site)),
   format("\n~a -> ", Site),
   page_links(Site, Links),                         
   maplist(domain_site, Links, Sites_Rep),
   sort(Sites_Rep, Sites),
   format("~w", [Sites]),
   maplist(add_new_site, Sites),
   recordz(visited_sites, site(Site)),
   fail.

% domain_site(+Domain, ?Site) is det
domain_site(Domain, Site) :-

  (
      % The Top domain is confirm our rules and 
      % is already present in top_level/1
      top_level(Domain, Top),
      top_level(Top)
  ;
      % May be it is an exception from top_level/2 rule
      top_level(Top)
  ;
      % There is not known top level for Domain
      % Try determine it by top_level/2
      top_level(Domain, Top),
      format("\nAdd the new top level: [~a]", Top),
      assertz(top_level(Top))
   ),

   atom_concat(Subdomain, Top, Domain), !,
   concat_atom(Subdomain_R, '.', Subdomain),
   reverse(Subdomain_R, [Site_Part|_]),
   atom_concat(Site_Part, Top, Site), !

   ; Site = Domain.

                                
% page_link(+Page, ?Link_To_Site) is nondet
page_link(Page, Link_To_Site) :-

%   http_get(Page, Html, []),

   http_page(Page, http_ops:http_get_html([], _), Page_Obj),
   obj_field(Page_Obj, dom, Html),
  
   xpath(Html, //a(@href), Href), 
   uri_is_global(Href), uri_components(Href, Comps), 
   uri_data(scheme, Comps, http),
   uri_data(authority, Comps, Link_To_Site).

% page_links(+Page, -Site_List) is det
page_links(Page, Site_List) :-

   catch(
         findall(Link, page_link(Page, Link), Link_List),
         Exception,
         (format("\n~w while call to [~a]", [Exception, Page]),
          Link_List = []
         )
        ),
  sort(Link_List, Site_List).

