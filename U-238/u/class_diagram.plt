:- begin_tests(class_diagram).
:- use_module(u(class_diagram)).

test(class_path, all(PAs == [[man_v, citizen_v, callup_v]])) :-
   class_path(man_v, callup_v, true, PAs).

:- end_tests(class_diagram).