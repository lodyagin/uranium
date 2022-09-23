:- module(html_tag_li_v,
          []).

/** <module> List item.

  ==
  <!ELEMENT LI    - O %flow>
  <!ATTLIST LI
        %SDAFORM; "LItem"
        >

  <!-- <LI>               List item                       -->
  ==
*/

:- use_module(u(v)).
:- use_module(u(ixpath)).
:- use_module(html_tag_v).

new_class(html_tag_li_v, html_tag_v,
          [
           ]).

downcast(html_tag_v, html_tag_li_v, From, To) :-
   gen_html_tag_downcast(From, To).

