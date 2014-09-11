:- module(nfa,
	  [
	   nfa_dfa/2
	  ]).

:- use_module(library(ugraphs)).
:- use_module(library(ordsets)).

:- dynamic arc/3, dfa_arc/3, state/3, egraph/1, dfa_state/3.


nfa_dfa(nfa(_, NFA_Arcs, NFA_S0, NFA_Final),
	dfa(DFA_States, DFA_Arcs, DFA_Initial, DFA_Final)
       ) :-

	% Initialize DB
	retractall(egraph(_)),
	retractall(arc(_, _, _)),
	retractall(dfa_arc(_, _, _)),
	retractall(state(_, _, _)),
	retractall(dfa_state(_, _, _)),

	forall(member(Arc, NFA_Arcs), assertz(Arc)),

	% assert e-graph for closure calculation
	assert_egraph,

	% calculate and assert the closure of start state
	closure(NFA_S0, S0_Closure),
	S0_Closure_Term =.. [compound_state|S0_Closure],
	assertz(state(S0_Closure_Term, 0, start)),

	% the main part
	list_to_ord_set(NFA_Final, NFA_Final_Set),
	resolve_unmarked_states(NFA_Final_Set),

	state_renumbering(DFA_States, DFA_Arcs,
			  DFA_Initial, DFA_Final0),

	% check whether the start state is also final
	(   memberchk(NFA_S0, NFA_Final)
	->  DFA_Final = [DFA_Initial|DFA_Final0]
	;   DFA_Final = DFA_Final0
	).


state_renumbering(States, Arcs, Initial, Final) :-

	% enumerate new states
	findall(dfa_state(Old, New, M),
		state(Old, _, M),
		States_C),
	bagof(New,
	      Old^M^member(dfa_state(Old, New, M), States_C),
	      States),
	length(States, N_States),
	numlist(1, N_States, States),
	% assert the translation table
	forall(member(S, States_C), assertz(S)),

	% renumerate states in arcs
	findall(
	    arc(New_From, Sym, New_To),
	    (   dfa_arc(Old_From, Sym, Old_To),
		dfa_state(Old_From, New_From, _),
		dfa_state(Old_To, New_To, _)
	    ),
	    Arcs ),

	dfa_state(_, Initial, start),
	!, % only one start state is possible

	findall(S, dfa_state(_, S, final), Final).



resolve_unmarked_states(NFA_Final) :-

	(
	    retract(state(T, 0, M))
	->
	    assertz(state(T, 1, M)),  % mark the state
	    process_transitions(NFA_Final, T),
	    resolve_unmarked_states(NFA_Final)
	;
	    true
	).

process_transitions(NFA_Final, T) :-

	T =.. [_|NFA_States],
	(   next_dest(NFA_States, Dest, Symbol),
	    closures(Dest, Closure),
	    (	ord_intersect(NFA_Final, Closure)
	    ->	M = final
	    ;	M = o
	    ),
	    Closure_Term =.. [compound_state|Closure],
	    (   state(Closure_Term, _, _)
	    ->  true
	    ;   assertz(state(Closure_Term, 0, M)) %add unmarked
	    ),
	    assert(dfa_arc(T, Symbol, Closure_Term)),
	    fail % next_dest
	;   true
	).

next_dest(States, Dest, Symbol) :-

	setof(D,
	      S^(  member(S, States),
		   arc(S, Symbol, D),
		   Symbol > -1
	      ),
	      Dest).


% Assert ugraph of all vertices participating in e-transitions

assert_egraph :-

	findall(X - Y, arc(X, -1, Y), Edges),
	vertices_edges_to_ugraph([], Edges, Graph),
	assert(egraph(Graph)).

closure(S, Closure) :-

	egraph(Graph), !,
	(   Graph == []
	->  Closure = [S]
	;   reachable(S, Graph, Closure0)
	->  list_to_ord_set([S|Closure0], Closure)
	;   Closure = [S]
	).

closures(State_Set, Closure) :-

	egraph(Graph), !,
	(   Graph == []
	->  Closure = State_Set
	;
	findall(C,
		(   member(S, State_Set),
		    reachable(S, Graph, C)
		),
		Closure0),
	append([State_Set|Closure0], Closure1),
	list_to_ord_set(Closure1, Closure)
	).
















