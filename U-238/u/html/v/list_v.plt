:- begin_tests(list_v).
:- use_module(u(v)).

test(option_list1, [OL =@= _]) :-

   obj_construct(list_v, [], [], L),
   obj_field(L, option_list, OL).

test(option_list2, [OL =@= _]) :-

   obj_construct(list_v, [definition_list], [[a, b]], L),
   obj_field(L, option_list, OL).

test(option_list3, [OL =@= _]) :-

   obj_construct(list_v, [value_list], [[1, 2]], L),
   obj_field(L, option_list, OL).

test(option_list4, [OL =@= _]) :-

   obj_construct(list_v,
                 [definition_list, value_list],
                 [[a, b, c], [1, 2]], L),
   obj_field(L, option_list, OL).

test(option_list5, [OL =@= [a=1, b=2]]) :-

   obj_construct(list_v,
                 [definition_list, value_list],
                 [[a, b], [1, 2]], L),
   obj_field(L, option_list, OL).

:- end_tests(list_v).
