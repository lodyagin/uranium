% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(ixpath, [ixpath/3,
                   ixpath/4,
                   op(950, fx, //),
                   op(950, fx, /),
                   op(200, fy, @),
                   op(150, xfx, ::)
                   ]).

/** <module> Uranium XPath implementation.

  In the accordance with http://www.w3.org/TR/xpath/ ,
  version 1.0.

  ---+++ Supported syntax

  Spec is defined as Prolog term with support of
  following operations:
   * //
   * /
   * @
   * ::

  *|Instead of '[' and ']' '(' and ')' should be used.|*

  ==
  [1] LocationPath ::=	  RelativeLocationPath
			| AbsoluteLocationPath

  [2] AbsoluteLocationPath ::= '/' RelativeLocationPath?
                       | AbbreviatedAbsoluteLocationPath
  ==

  *|The empty RelativeLocationPath is not implemented|*

  ==
  [3] RelativeLocationPath	   ::=		Step
			| RelativeLocationPath '/' Step
			| AbbreviatedRelativeLocationPath

  [4] Step ::=		AxisSpecifier NodeTest Predicate*
			| AbbreviatedStep
  ==

  TODO

  ==
  [5] AxisSpecifier   ::=	AxisName '::'
			| AbbreviatedAxisSpecifier

  [6] AxisName	   ::=    'ancestor'
			| 'ancestor-or-self'
			| 'attribute'
			| 'child'
			| 'descendant'
			| 'descendant-or-self'
			| 'parent'
			| 'self'
  ==

  *|Defined by the w3c recommendations following,
  following-sibling, namespace, preceding,
  preceding-sibling axes are not supported|*

  ==
  [10] AbbreviatedAbsoluteLocationPath ::=
                       '//' RelativeLocationPath

  [11] AbbreviatedRelativeLocationPath	   ::=
                       RelativeLocationPath '//' Step

  [12] AbbreviatedStep ::= '.' | '..'
  ==

  *|reverse_axis ('..') is not supported|*

  ==
  [13] AbbreviatedAxisSpecifier ::= '@'?
  ==

  TODO: complete the syntax description
*/

:- use_module(library(lists)).
:- use_module(u(v)).
:- use_module(u(ur_option)).
:- use_module(u(internal/check_arg)).
:- use_module(u(internal/objects_i)).
:- use_module(u(ur_messages)).

:- reexport(u(ur_messages), [prolog:message//1]).


%% ixpath(+Spec, +Dom, -Result)
%
% It is like ixpath/4 but with Options = []

ixpath(Spec, Dom, Result) :-

   Ctx = context(ixpath/3, _),
   context_module(That),
   ixpath_cmn(Spec, That:[], Dom, Result, Ctx).


%% ixpath(+Spec, +Options, +Dom, -Result)
%
% Select the node by xpath. The default form of Result is
% an element/3 term, see http://www.swi-prolog.org/pldoc/doc_for?object=section(0,%270%27,swi(%27/doc/packages/sgml.html%27))
%
% @param Spec the specification in the format defined
% above
%
% @param Options
%  $ v : instead of element/3 unify Result with a
%  html_piece_v descendant (auto downcasted) with the dom
%  field unified with the actual element/3 found by xpath.
%
%  $ vx : the same but v but also unifies node_path,
%  node_rpath and xpath fields
%
%    $ xpath : is Prolog atom for selected xpath with a
%    standard (w3c recommendation) syntax. Return an axis
%    index for elements with defined nesting.
%
%    $ node_path : list of nodes. The nodes are
%    represented as node/2 elements. The first arg is
%    number of node in the axis (1-based), the second is
%    element/3 for the node. The list is started from the
%    Dom node and ended with the Result node.
%
%    $ node_rpath : the same as node_path but started from
%    the result node and ended with the Dom node.
%
%  $ vix : the same as vx but Result's xpath is unified
%  with xpath in the format of this library (the same
%  syntax as for Spec parameter).
%
%  $ vixc: the same as vix but ensure there is a position number
%  for each tag in xpath, e.g.:
%  /html(1)/body(1)/div(1)/div(2)/table(3)
%
% @param Dom it can be element/3 term, Uranium object with
% a `dom' field or a list with single element/3 member
%

:- meta_predicate ixpath(+, :, +, -).

ixpath(Spec, Options, Dom, Result) :-

   Ctx = context(ixpath/4, _),
   ixpath_cmn(Spec, Options, Dom, Result, Ctx).

ixpath_cmn(Spec, Options0, Dom, Result, Ctx) :-
   options_to_object(ixpath, Options0, Options),
   ixpath2(Spec, Options, Dom, Result, Ctx).


ixpath2(Spec, Options, LDOM, Result, Ctx) :-

   nonvar(LDOM), is_list(LDOM), !,
   member(DOM, LDOM),
   ixpath2(Spec, Options, DOM, Result, Ctx).

ixpath2(Spec, Options, Obj, Result, Ctx) :-

   u_object(Obj), !,
   obj_field(Obj, dom, DOM),
   ixpath2(Spec, Options, DOM, Result, Ctx).

ixpath2(Spec, Options, Dom, Result, Ctx) :-

   check_inst(Dom, Ctx),
   functor(Dom, element, 3), !,
   (  nonvar(Spec) -> true
   ;  throw(error(instantiation_error, Ctx))
   ),
   w3c_xpath(Spec, Expr),
   xpath(Expr, Dom, Options, Result).

ixpath2(_, _, Dom, _, Ctx) :-

   throw(error(type_error(dom, Dom), Ctx)).


ixpath_to_xpath(I, X) :-

   term_to_atom(I, IA),
   atom_chars(IA, IAL),
   selectchk('(', IAL, '[', IAL1),
   selectchk(')', IAL1, ']', IAL2),
   atom_chars(X, IAL2).

term_list_to_ixpath([Term], Term) :- !.

term_list_to_ixpath([T1|Tail], Head/T1) :-

   term_list_to_ixpath(Tail, Head).


term_list_to_xpath([]) --> [].

term_list_to_xpath([T1|Tail]) -->

   "/", term_to_xpath(T1),
   term_list_to_xpath(Tail).

term_to_xpath(Term) -->

   { atom(Term), !, atom_codes(Term, Codes) },
   Codes.

term_to_xpath(Term) -->

   { Term =.. [Functor|Args], atom_codes(Functor, Codes) },
   Codes, "[", args_to_xpath(Args), "]".

args_to_xpath([Arg]) -->

   { !, atom_codes(Arg, Codes) }, Codes.

args_to_xpath([Arg|T]) -->

   { atom_codes(Arg, Codes) },
   Codes, ",",
   args_to_xpath(T).

% w3c xpath spec:
%
% [1] LocationPath ::=		RelativeLocationPath
%			| AbsoluteLocationPath

w3c_xpath(Path, P) :-

   (  w3c_absolute_xpath(Path, P), !
   ;  w3c_relative_xpath(Path, _, P) ).


% [2] AbsoluteLocationPath ::= '/' RelativeLocationPath?
%                              | AbbreviatedAbsoluteLocationPath
%
% [10] AbbreviatedAbsoluteLocationPath::='//' RelativeLocationPath

w3c_absolute_xpath((/), root) :-

   throw(ixpath_not_implemented(root_node, _)).

w3c_absolute_xpath(/Rel_Path, root(P)) :- !,

   w3c_relative_xpath(Rel_Path, _, P).

w3c_absolute_xpath(//Rel_Path, root(P)) :- !,

   w3c_relative_xpath(Rel_Path, descendant, P).

% [3]	RelativeLocationPath	   ::=		Step
%			| RelativeLocationPath '/' Step
%			| AbbreviatedRelativeLocationPath
%
% [11]  AbbreviatedRelativeLocationPath	   ::=
%                          RelativeLocationPath '//' Step

w3c_relative_xpath(Rel_Path/Step, First_Axis, P1/P2) :- !,

   w3c_relative_xpath(Rel_Path, First_Axis, P1),
   w3c_step(Step, P2).

w3c_relative_xpath(Rel_Path//Step, First_Axis, P1/P2) :- !,

   w3c_relative_xpath(Rel_Path, First_Axis, P1),
   w3c_step(descendant::Step, P2).

w3c_relative_xpath(Step, First_Axis, P) :-

   (  nonvar(First_Axis) % only for abbrev syntax
   -> w3c_step(First_Axis::Step, P)
   ;  w3c_step(Step, P) ).

% [4] Step ::=		AxisSpecifier NodeTest Predicate*
%			| AbbreviatedStep
% [5] AxisSpecifier   ::=	AxisName '::'
%			| AbbreviatedAxisSpecifier
%
% [13] AbbreviatedAxisSpecifier	   ::=		'@'?
%
% [12]		AbbreviatedStep	   ::=		'.'
%			| '..'


w3c_step(Axis_Name::Term, Axis_Name::P) :- !,

   w3c_axis_name(Axis_Name), !,
   preprocess_expr(Term, P).

% abbreviated syntax extension

w3c_step(@Atom, attribute::Atom) :- !,

   atom(Atom).

w3c_step('.', self::tag('*', _, _)) :- !.

% reverse axis is not implemented
w3c_step('..', _) :-

   throw(ixpath_not_implemented(reverse_axis, _)).

w3c_step(Term, P) :- w3c_step(child::Term, P).

% [6]		AxisName	   ::=		'ancestor'
%			| 'ancestor-or-self'
%			| 'attribute'
%			| 'child'
%			| 'descendant'
%			| 'descendant-or-self'
%			| 'following'
%			| 'following-sibling'
%			| 'namespace'
%			| 'parent'
%			| 'preceding'
%			| 'preceding-sibling'
%			| 'self'
% TODO

w3c_axis_name(ancestor).
w3c_axis_name(ancestor-or-self).
w3c_axis_name(attribute).
w3c_axis_name(child).
w3c_axis_name(descendant).
w3c_axis_name('descendant-or-self').
w3c_axis_name(parent).
w3c_axis_name(self).


% preprocess_expr(+Expr, -Preprocessed_Expr)
%
% build
% 1) unabbreviated
% 2) easy to calc form
%
% preprocess_expr process Node_Test Predicate in form
%
% Node_Name | Node_Name(Test {, Test})

preprocess_expr(Node_Name, tag(Node_Name, _, _)) :-

   atom(Node_Name), !.

preprocess_expr(Node_Name, tag(Tag, Num, Attrs)) :-

   compound(Node_Name),
   Node_Name =.. [Tag|Cond_List],
   preprocess_cond(Cond_List, [], Attrs_R, _, Num), !,
   reverse(Attrs_R, Attrs).

preprocess_cond([], Attrs, Attrs, Num, Num) :- !.

preprocess_cond([@Attr=Value|T], Attrs0, Attrs, Num0,Num) :-

   !,
   atom(Attr),
   preprocess_cond(T, [Attr=Value|Attrs0], Attrs, Num0, Num).

preprocess_cond([attribute::Attr=Value|T], Attrs0, Attrs,
                Num0, Num) :-

   !,
   atom(Attr),
   preprocess_cond(T, [Attr=Value|Attrs0], Attrs, Num0, Num).

preprocess_cond([Num|T], Attrs0, Attrs, Num0, Num) :-

   var(Num0), % only one num spec per element allowed
   integer(Num), Num > 0, !,
   preprocess_cond(T, Attrs0, Attrs, Num, Num).


xpath(Spec, Dom, Options0, Result) :-

   obj_rewrite(Options0, [v], [V0], [V], Options),
   V0 == vixc, !,
   V = vix,
   findall(R,
	   ( copy_term(Spec, Spec1),
	     xpath2(Spec1, Dom, Options, R)
	   ),
	   Result1
	  ),
   setof(XPath,
         R^( member(R, Result1),
             obj_field(R, xpath, XPath)
           ),
         XPathes
         ),
   member(XPath1, XPathes),
   w3c_xpath(XPath1, Spec1),
   xpath2(Spec1, Dom, Options, Result).

xpath(Spec, Dom, Options, Result) :-

   xpath2(Spec, Dom, Options, Result).

xpath2(Spec, Dom, Options, Result) :-

   bagof(pair(Path, Path_R, Result1, Spec1),
	 ( copy_term(Spec, Spec1),
	   xpath(Spec1, Dom, [], Path_R, Result1),
	   reverse(Path_R, Path) ),
	 Results_Dup_U),

   % remove dups and unify element counters
   msort(Results_Dup_U, Results_Dup),
   unify_list_pairs(Results_Dup, Results_R),
   reverse(Results_R, Results),

   % bt
   member(pair(Path, Path_R, Result1, Spec), Results),
   proc_options(Path, Path_R, Options, Result1, Result).

unify_list_pairs([], []) :- !.
unify_list_pairs([One], [One]) :- !.
unify_list_pairs([El, El|T1], [El|T2]) :- !,
   unify_list_pairs(T1, T2).
unify_list_pairs([El|T1], [El|T2]) :-
   unify_list_pairs(T1, T2).


proc_options(Path, Path_R, Options, Result0, Result) :-
   obj_field(Options, v, V),
   ( var(V) -> Result = Result0
   ; proc_option(V, Path, Path_R, Result0, Result)
   ).

proc_option(v, _, _, Result0, Result) :- !,
   obj_construct(html_piece_v, [dom], [Result0], Result1),
   obj_downcast(Result1, Result).

proc_option(vx, Path, Path_R, Result0, Result) :- !,
   proc_option(xpath(XPath), Path, Path_R, _, _),
   obj_construct(html_piece_v,
                 [dom, node_path, node_rpath, xpath],
                 [Result0, Path, Path_R, XPath],
                 Result1),
   obj_downcast(Result1, Result).

proc_option(vix, Path, Path_R, Result0, Result) :- !,
   proc_option(ixpath(XPath), Path, Path_R, _, _),
   obj_construct(html_piece_v,
                 [dom, node_path, node_rpath, xpath],
                 [Result0, Path, Path_R, XPath],
                 Result1),
   obj_downcast(Result1, Result).

proc_option(tag_path(Tag_Path), Path, _, R, R) :- !,
   dom_tag_path(Path, Tag_Path).

proc_option(tag_path_cnt(Tag_Path), Path, _, R, R) :- !,
   dom_tag_path_cnt(Path, Tag_Path).

proc_option(xpath(XPath), Path, _, R, R) :- !,
   dom_tag_path_cnt(Path, Term_List),
   phrase(term_list_to_xpath(Term_List), XPath_Codes, []),
   atom_codes(XPath, XPath_Codes).

proc_option(ixpath(/XPath), _, Path_R, R, R) :- !,
   dom_tag_path_cnt(Path_R, Term_List),
   term_list_to_ixpath(Term_List, XPath).

proc_option(tag_attr_path(Attr_List, Tag_Attr_Path), Path, _,
            R, R) :- !,
   dom_tag_attr_path(Path, Attr_List, Tag_Attr_Path).

proc_option(_, _, _, R, R) :- true.


% DOM elements path -> tags path
dom_tag_path([], []) :- !.
dom_tag_path([node(_, element(Tag, _, _), _)|DT], [Tag|TT]) :-
   dom_tag_path(DT, TT).

dom_tag_path_cnt([], []) :- !.
dom_tag_path_cnt([node(_, element(Tag, _, _), Cnt)|DT], [Term|TT]) :-
   nonvar(Cnt), !,
   Term =.. [Tag, Cnt],
   dom_tag_path_cnt(DT, TT).
dom_tag_path_cnt([node(_, element(Tag, _, _), _)|DT], [Tag|TT]) :-
   dom_tag_path_cnt(DT, TT).

% DOM elements path -> tags with selected attrs path
dom_tag_attr_path([], _, []) :- !.
dom_tag_attr_path([node(_, element(Tag, Attrs, _), _)|DT],
                  Attrs_Query,
                  [Tag_With_Attrs|TT]) :-

   extract_attrs(Attrs_Query, Attrs, Attr_Vals),
   Tag_With_Attrs =.. [Tag|Attr_Vals],
   dom_tag_attr_path(DT, Attrs_Query, TT).

% TODO O(N*M) -> use uranium objects
extract_attrs([], _, []) :- !.
extract_attrs([Attr|AQT], Attrs, [Val|AVT]) :-
   (  memberchk(Attr=Val, Attrs)
   -> true
   ;  Val = '' ),
   extract_attrs(AQT, Attrs, AVT).

% xpath essentials

xpath(root(XPath), Dom, Path0, Path, Result) :- !,

   Dummy_Root = element(dummy_root, [], [Dom]),
   xpath(XPath, Dummy_Root, Path0, Path, Result).

xpath(Head/Step, Dom, Path0, Path, Result) :- !,

   xpath(Head, Dom, Path0, Path1, Result1),
   step(Step, Result1, Path1, Path, Result).

xpath(Step, Dom, Path0, Path, Result) :-

   step(Step, Dom, Path0, Path, Result).

step(ancestor::Node_Test, Dom, Path0, Path, Result) :- !,

   step(parent::tag('*', _, _), Dom, Path0, Path1, Result1),
   step('ancestor-or-self'::Node_Test, Result1, Path1, Path,
	Result).

step('ancestor-or-self'::tag(_, M, _), _, _, _, _) :-

   nonvar(M),
   throw(ixpath_not_implemented(axis_counter(ancestor), _)).

step('ancestor-or-self'::Node_Test, Dom, Path0, Path, Result) :- !,

   (   step(self::Node_Test, Dom, Path0, Path, Result)
   ;   step(ancestor::Node_Test, Dom, Path0, Path, Result)
   ).

step(attribute::(*), Dom, Path, Path, Attrs) :- !,

   Dom = element(_, Attrs, _).

step(attribute::Attr, Dom, Path, Path, Result) :- !,

   Dom = element(_, Attrs, _),
   memberchk(Attr=Result, Attrs).

step(child::Node_Test, Dom, Path, [Child_Node|Path], R) :- !,

   Dom = element(_, _, Sub_Elements),
   Node_Test = tag(Tag, M, _),

   child_member_test(Node_Test, M, 1, 1, Sub_Elements,
		     node(Idx, R, Cnt)),

   (  Tag == '*' ->
      Child_Node = node(Idx, R, _) % Cnt is meaningfull only for
                               % named tags
   ;  Child_Node = node(Idx, R, Cnt) ).

step(descendant::Node_Test, Dom, Path0, Path, Result) :- !,

   step(child::tag('*', _, _), Dom, Path0, Path1, Result1),
   step('descendant-or-self'::Node_Test, Result1, Path1, Path,
        Result).

step('descendant-or-self'::tag(_, M, _), _, _, _, _) :-

   nonvar(M),
   throw(ixpath_not_implemented(axis_counter(descendant), _)).

step('descendant-or-self'::Node_Test, Dom,Path0,Path, Result) :- !,

   (  step(self::Node_Test, Dom, Path0, Path, Result)
   ;  step(descendant::Node_Test, Dom, Path0, Path, Result)
   ).

step(parent::Node_Test, _, [_, Parent|PT], [Parent|PT], R) :- !,

   Parent = node(_, Node, _),
   step(self::Node_Test, Node, _, _, R).

step(self::tag('*', _, Attrs), Dom, Path, Path, Dom) :- !,

   check_attrs(Attrs, Dom).

step(self::tag(Tag, _, Attrs), Dom, Path, Path, Dom) :- !,

   Dom = element(Tag, _, _),
   check_attrs(Attrs, Dom).

step(self::tag('*', _, _), Dom, Path, Path, Dom) :- !.

step(self::tag(Tag, _, _), Dom, Path, Path, Dom) :- !,

   Dom = element(Tag, _, _).


% Node_Test - the test expression
% M - if bound - the number of tag from the matched set
% I - current tag number
% Cnt - number of matched nodes - 1

child_member_test(Node_Test, M, I, Cnt, [Child|T], Matched_Node) :-
   ( var(M) -> true ; Cnt < M ),

   (  functor(Child, element, 3)
   -> (  I1 is I + 1,
         (  step(self::Node_Test, Child, _, _, Result)
         ->
            (  Matched_Node = node(I, Result, Cnt)
            ;  Cnt1 is Cnt + 1,
               child_member_test(Node_Test, M, I1, Cnt1, T, Matched_Node)
            )
         ;
            child_member_test(Node_Test, M, I1, Cnt, T, Matched_Node)
         )
      )
   ;
      % skip not element/3 parts (like simple text)
      child_member_test(Node_Test, M, I, Cnt, T, Matched_Node)
   ).


% check conditions

check_attrs([], _) :- !.
check_attrs([Attr=Value|TC], Result) :-
   Result = element(_, Attributes, _),
   memberchk(Attr=Value, Attributes),
   check_attrs(TC, Result).

