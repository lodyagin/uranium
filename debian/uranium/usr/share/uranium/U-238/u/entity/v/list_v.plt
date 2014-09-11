:- begin_tests(list_v).
:- use_module(u(v)).

test(option_list1, [OL =@= _]) :-

   obj_construct(definition_list_v, [], [], L),
   obj_field(L, option_list, OL).

test(option_list2, [OL =@= []]) :-

   obj_construct(definition_list_v, [value_list], [[a, [b]]], L),
   obj_field(L, option_list, OL).

test(option_list3, [OL =@= [b=1, c=[2,3]]]) :-

   obj_construct(definition_list_v,
                 [value_list],
                 [[[a],
                  [b, 1],
                  [c, 2, 3],
                  d
                 ]],
                 L),
   obj_field(L, option_list, OL).

test(option_list4, [OL =@= [a=1, b=2]]) :-

   obj_construct(definition_list_v,
                 [value_list],
                 [[[a, 1], [b, 2]]], L),
   obj_field(L, option_list, OL).

:- end_tests(list_v).
