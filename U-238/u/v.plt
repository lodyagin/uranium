:- begin_tests(v).
:- use_module(u(v)).
:- use_module(u(internal/objects_i)).
:- use_module(u(http/v/http_user_v)).

test(class_create1) :-
   % test class_create/3 version

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b]),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   class_fields(citizen_v, CF), sort(CF, CFS),
   class_fields(class_create_test_v, CCTF), sort(CCTF, CCTFS),
   class_fields_new(class_create_test_v, CCTFN),
   assertion(CF == CFS),
   assertion(CCTF = CCTFS),
   assertion(CCTFN == [a, b, c]),

   ord_subtract(CCTF, CF, New_Fields),
   assertion(New_Fields == CCTFN),

   obj_field(CCTO, age, Age),
   integer(Age),

   obj_key(CCTO, Key),
   assertion(Key == [id]).

test(class_create2) :-
   % test class_create/4 version

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b],
                [birthday, a]),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   class_fields(citizen_v, CF), sort(CF, CFS),
   class_fields(class_create_test_v, CCTF), sort(CCTF, CCTFS),
   class_fields_new(class_create_test_v, CCTFN),
   assertion(CF == CFS),
   assertion(CCTF = CCTFS),
   assertion(CCTFN == [a, b, c]),

   ord_subtract(CCTF, CF, New_Fields),
   assertion(New_Fields == CCTFN),

   obj_field(CCTO, age, Age),
   integer(Age),
   
   obj_key(CCTO, Key),
   assertion(Key == [a, birthday]).

test(class_create_feature1) :-
   % It is a strange feature

   current_prolog_flag(verbose, Old_Verbose),
   set_prolog_flag(verbose, silent),
   reload_all_classes,
   set_prolog_flag(verbose, Old_Verbose),

   class_create(class_create_test_v, citizen_v, [a, c, b], []),
   obj_construct(class_create_test_v, [birthday], [1976], CCTO),
   obj_key(CCTO, Key),
   assertion(Key == []).

% test(class_descendant1,
%      [List == [callup_v, citizen_v, class_create_test_v]]
%     ) :-

%   findall(X, class_descendant(man_v, X), List0),
%   msort(List0, List).

% test(class_descendant2, [List == []]) :-

%   findall(X, class_descendant(callup_v, X), List).

% test(class_descendant3, [List == []]) :-

%   obj_construct(tarjan_vertex_v, [], [], V),
%   obj_rebase((object_v -> callup_v), V, _),
%   findall(X, class_descendant(callup_v, X), List).

% test(class_same_or_descendant1,
%      [List == [callup_v, citizen_v, class_create_test_v, man_v]]
%     ) :-

%   findall(X, class_same_or_descendant(man_v, X), List0),
%   msort(List0, List).

% test(class_same_or_descendant2,
%      [List == [callup_v]]
%     ) :-

%   findall(X, class_same_or_descendant(callup_v, X), List).

% test(class_same_or_descendant3, [List == [callup_v]]) :-

%   obj_construct(tarjan_vertex_v, [], [], V),
%   obj_rebase((object_v -> callup_v), V, _),
%   findall(X, class_same_or_descendant(callup_v, X), List).

test(class_parent1, [Parent == man_v]) :-

   class_parent(citizen_v, Parent).

test(class_parent2,
     [L == [callup_v, class_create_test_v]]) :-

   findall(Desc, class_parent(Desc, citizen_v), LU),
   msort(LU, L).

test(class_parent3) :-

   findall(p(A, B), class_parent(A, B), L),
   length(L, N),
   assertion(N > 10).

test(eval_obj_expr,
     [[E1, E2, E3] =@= [HTTP_Result, WWW_Addr, Url]]
     ) :-

   Url = 'http://kogorta.dp.ua',
   obj_construct(www_address_v,
                 [http_request_url], [Url], WWW_Addr),
   obj_construct(http_result_v,
                 [www_address], [WWW_Addr], HTTP_Result),
   eval_obj_expr(HTTP_Result, E1),
   eval_obj_expr(HTTP_Result/www_address, E2),
   eval_obj_expr(HTTP_Result/www_address/http_request_url, E3).

test(obj_construct_with_evals1) :-

    obj_construct(citizen_v,
                  [functor, class, birthday],
                  [citizen_v, citizen_v, 1994], O1),
    obj_field(O1, class, Class1),
    assertion(Class1 == citizen_v),
    
    obj_construct(citizen_v,
                  [birthday, functor, class],
                  [1994, citizen_v, citizen_v], O2),
    obj_field(O2, class, Class2),
    assertion(Class2 == citizen_v),
    
    obj_construct(citizen_v,
                  [birthday, sex, functor, class],
                  [1994, man, citizen_v, callup_v], O3),
    obj_field(O3, class, Class3),
    assertion(Class3 == callup_v),

    obj_construct(citizen_v,
                  [functor, class, birthday, sex],
                  [citizen_v, callup_v, 1994, man], O4),
    obj_field(O4, class, Class4),
    assertion(Class4 == callup_v),

    obj_construct(citizen_v,
                  [functor, class, birthday, sex],
                  [X, Y, 1994, man], O5),
    obj_field(O5, class, Class5),
    assertion(Class5 == callup_v),
    assertion(X = citizen_v),
    assertion(Y = callup_v).

    
test(obj_construct_bug1) :-

   class_fields(man_v, Field_Names),
   obj_construct(man_v, Field_Names, Field_Names, Obj),
   Obj =.. [man_v, _|Field_Names2],

   assertion(Field_Names == Field_Names2).

test(obj_copy1) :-

   obj_construct(man_v, [], [], M1),
   obj_copy(M1, M2),
   obj_field(M1, name, 'Masha'),
   obj_field(M2, name, 'Vitya').

test(obj_copy2) :-

   new_http_user(U1),
   obj_copy(U1, U2),
   obj_field(U1, cookie_db_key, Key1),
   obj_field(U2, cookie_db_key, Key2),
   assertion(Key1 \= Key2).

test(obj_diff1,
     [Diff == [diff(name, 'Sergei', 'Artemiy')]]) :-

   obj_construct(man_v, [name, surname], ['Sergei', 'Lodyagin'], M1),
   obj_construct(man_v, [name, surname], ['Artemiy', 'Lodyagin'], M2),
   obj_diff(M1, M2, Diff).

test(obj_diff2,
     [Diff == [diff(name, 'Sergei', 'Artemiy')]]) :-

   obj_construct(man_v, [name, surname], ['Sergei', 'Lodyagin'], M1_0),
   obj_rebase((object_v -> db_object_v), M1_0, M1),
   obj_construct(man_v, [name, surname], ['Artemiy', 'Lodyagin'], M2),
   obj_diff(M1, M2, Diff).
  

% After some time it will always fails (because it depends
% on the current year).
test(obj_downcast1,
     [C1 =@= callup_v(Class_Id, 1994, _, _, _, _, _, man, _, _)]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], C0),
   obj_downcast(C0, C1),
   arg(1, C1, Class_Id).

test(obj_field1, [Flds == Vals]) :-

   class_fields(citizen_v, Flds),
   obj_construct(citizen_v, Flds, Flds, Obj),
   findall(Val, (obj_field(Obj, Fld, Val), member(Fld, Flds)), Vals).

test(obj_field2, [Flds3 == Flds4]) :-
% Each eval field must be evaluable only once

   class_fields(citizen_v, Flds),
   obj_construct(citizen_v, Flds, Flds, Obj),
   findall(Fld, obj_field(Obj, Fld, _), Flds2),
   sort(Flds2, Flds3),
   msort(Flds2, Flds4).

test(obj_field3, [Surname == 'Grisha']) :-

   obj_construct(man_v, [], [], Man),
   obj_field(Man, name, Name),
   obj_field(Man, surname, Name),
   Name = 'Grisha',
   obj_field(Man, surname, Surname).
  
test(obj_field_bug1, [fail]) :-

   obj_construct(citizen_v, [sex], [woman], Obj),
   obj_field(Obj, sex, man).

test(obj_is_descendant1, [List == [object_base_v, object_v]]) :-

   obj_construct(man_v, [], [], Man),
   findall(X, obj_is_descendant(Man, X), List0),
   msort(List0, List).

test(obj_is_descendant2) :-

   obj_construct(man_v, [], [], Man),
   \+ obj_is_descendant(Man, db_object_v),
   obj_rebase((object_v -> db_object_v), Man, Man2),
   obj_is_descendant(Man2, db_object_v).

test(obj_set_field1, [Surname =@= _]) :-

   obj_construct(man_v, [], [], Man),
   obj_set_field(Man, name, Name),
   obj_set_field(Man, surname, Name),
   Name = 'Grisha',
   obj_field(Man, surname, Surname).
  
test(obj_set_field2, [Surname =@= 'Grisha']) :-

   obj_construct(man_v, [name, surname], [Name, Name], Man),
   obj_set_field(Man, name, 'Grisha'),
   obj_field(Man, surname, Surname).
  
test(obj_key1) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Man1),
   obj_key(Man1, Key1),
   assertion(Key1 == [name, surname]),
   obj_key_value(Man1, Key_Value1),
   assertion(Key_Value1 =@= ['Adam', _]),

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man2),
   obj_key(Man2, Key2),
   assertion(Key2 == [id]),
   obj_key_value(Man2, Key_Value2),
   assertion(Key_Value2 =@= [_]).

test(obj_key2) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Man1_0),
   obj_rebase((object_v -> db_object_v), Man1_0, Man1),
   obj_key(Man1, Key1),
   assertion(Key1 == [name, surname]),
   obj_key_value(Man1, Key_Value1),
   assertion(Key_Value1 =@= ['Adam', _]),

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man2_0),
   obj_rebase((object_v -> db_object_v), Man2_0, Man2),
   obj_key(Man2, Key2),
   assertion(Key2 == [id]),
   obj_key_value(Man2, Key_Value2),
   assertion(Key_Value2 =@= [_]).


test(obj_list1,
     [List =@= [country = ['Soviet Union'],
                sex = man,
                surname = 'Mayakovsky']]
     ) :-

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man),
   obj_list(Man, List).
 
test(obj_list2, [List =@= []]) :-

   obj_construct(citizen_v, [], [], Man),
   obj_list(Man, List).

test(obj_parents1_1,
     [P = [citizen_v, man_v, object_v, object_base_v]]) :-

   obj_construct(citizen_v, [], [], V),
   obj_parents(V, P).

test(obj_parents2_1) :-

   obj_construct(citizen_v, 
                 [sex, surname, country], 
                 [man, 'Mayakovsky', ['Soviet Union']],
                 Man1),
   New_Parents_Order = [man_v, citizen_v, object_v, object_base_v],
   obj_parents(Man1, New_Parents_Order, Man2),
   obj_parents(Man1, P1),
   assertion(P1 == [citizen_v, man_v, object_v, object_base_v]),
   obj_parents(Man2, P2),
   assertion(P2 == New_Parents_Order),
   obj_list(Man1, L1),
   obj_list(Man2, L2),
   assertion(L1 == L2),
   obj_field(Man1, class, C1),
   obj_field(Man2, class, C2),
   obj_field(Man1, functor, F1),
   obj_field(Man2, functor, F2),
   assertion([F1, F2, C1, C2] == [citizen_v, man_v, callup_v, callup_v]).

test(obj_parents2_bug1, [error(use_rebase_to_insert_parents)]) :-

   obj_construct(man_v, [], [], M),
   obj_parents(M, [man_v, db_object_v, object_v, object_base_v], _).

test(obj_rebase1) :-

   obj_construct(http_request_headers_v, [], [], V1),
   obj_parents(V1, P1),
   assertion(P1 == [http_request_headers_v, http_headers_v,
                    object_v,object_base_v]),
   obj_rebase((http_headers_v -> http_response_headers_v),
              V1, V2),
   obj_parents(V2, P2),
   assertion(P2 == [http_request_headers_v,
                    http_response_headers_v,http_headers_v,
                    object_v,object_base_v]),
   obj_rebase((http_request_headers_v ->
               http_invalid_mixed_headers_v), V2, V3),
   obj_parents(V3, P3),
   assertion(P3 == [http_invalid_mixed_headers_v,
                    http_invalid_headers_v,
                    http_headers_v,
                    object_v,object_base_v]).

test(obj_rebase2_transitivity) :-

   obj_construct(citizen_v, [], [], M1),
   obj_rebase((object_v -> db_object_v), M1, M2),
   obj_rebase((man_v -> tarjan_vertex_v), M2, M3),
   obj_parents(M3, P3),
   assertion(P3 == [citizen_v, tarjan_vertex_v, db_object_v,
                    object_v, object_base_v]).

test(obj_rebase3_transitivity2) :-

   obj_construct(citizen_v, [], [], M1),
   obj_rebase((object_v -> db_object_v), M1, M2),
   obj_rebase((man_v -> tarjan_vertex_v), M2, M3),
   foreach(obj_field(M3, _, _), true).

test(obj_rebase_bug1) :-

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(man_v, [sex, name], [man, 'Adam'], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_rebase_bug2) :-

   obj_construct(citizen_v, [sex], [man], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [sex], [man], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_rebase_bug3_1) :-

   obj_construct(db_object_v, [], [], Obj1_0),
   obj_rebase((object_v -> db_object_v), Obj1_0, Obj1),

   obj_construct(db_object_v, [], [], Obj2_0),
   obj_rebase((object_v -> db_object_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((db_object_v -> object_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =\= Class_Id3),

   class_primary_id(object_v, Object_V_Id),
   assertion(Obj3 == object_v(Object_V_Id)).

test(obj_rebase_bug3_2) :-

   obj_construct(citizen_v, [], [], Obj1_0),
   obj_rebase((man_v -> citizen_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [], [], Obj2_0),
   obj_rebase((object_v -> man_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2).

test(obj_rebase_bug4,
     [throws(error(cant_rebase_to_object_base_v,
                   context(obj_rebase/3, _)))]) :-

   obj_construct(citizen_v, [], [], Obj1_0),
   obj_rebase((citizen_v -> object_base_v), Obj1_0, Obj1),

   obj_construct(citizen_v, [], [], Obj2_0),
   obj_rebase((citizen_v -> object_base_v), Obj2_0, Obj2),

   arg(1, Obj1, Class_Id1),
   arg(1, Obj2, Class_Id2),

   assertion(Class_Id1 =:= Class_Id2),

   obj_rebase((object_base_v -> citizen_v), Obj2, Obj3),

   arg(1, Obj2_0, Class_Id0),
   arg(1, Obj3, Class_Id3),
   assertion(Class_Id0 =:= Class_Id3),
   assertion(Obj2_0 == Obj3).

test(obj_reset_fields1) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields([name, height], Man0, Man1),
   obj_class_id(Man1, Class_Id),
   assertion(Man1 =@= man_v(Class_Id, _, _, man, _, 63)),
   
   obj_reset_fields([weight], Man1, Man2),
   assertion(Man2 =@= man_v(Class_Id, _, _, man, _, _)),
   
   obj_reset_fields([], Man2, Man3),
   assertion(Man3 =@= Man2).
   
test(obj_reset_fields2, [fail]) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields([name, height, age], Man0, _).
   
test(obj_reset_fields_weak) :-

   obj_construct(man_v,
                 [sex, name, weight, height],
                 [man, 'Simeon', 63, 1.75], Man0),
   
   obj_reset_fields_weak([name, height, age], Man0, Man1),
   obj_class_id(Man1, Class_Id),
   assertion(Man1 =@= man_v(Class_Id, _, _, man, _, 63)).

test(obj_reset_fields_with_evals, [fail]) :-

   obj_construct(citizen_v, [sex], [man], O),
   obj_reset_fields([age], O, _).

test(obj_rewrite) :-

   obj_construct(man_v,
                 [name, height, weight],
                 ['Luda', 1.40, 99], Luda1),
   obj_rewrite(Luda1,
               [height, weight, name],
               [Old_Height, Old_Weight, Name],
               [1.44, 69, Name],
               Luda2),
   assertion([Old_Height, Old_Weight] =@= [1.40, 99]),
   obj_rewrite(Luda2,
               [height, weight, name],
               Luda2_Pars,
               [1.45, 64, _],
               Luda3),
   assertion(Luda2_Pars =@= [1.44, 69, 'Luda']),
   named_args_unify(Luda3, [height, weight, name], Luda3_Pars),
   assertion(Luda3_Pars =@= [1.45, 64, _]).

test(obj_rewrite_weak1,
     [throws(error(no_object_field(Luda1, country), _))]) :-

   obj_construct(man_v,
                 [name, height, weight],
                 ['Luda', 1.40, 99], Luda1),
   obj_rewrite(Luda1,
               [height, weight, name, country],
               _,
               [1.44, 69, _, 'UK'],
               _).

test(obj_rewrite_weak2,
     [[Old_Height, Old_Weight, Name, Old_Country]
     =@= [1.40, 99, 'Luda', _]]
     ) :-

   obj_construct(man_v,
                 [name, height, weight],
                 ['Luda', 1.40, 99], Luda1),
   obj_rewrite(Luda1, weak,
               [height, weight, name, country],
               [Old_Height, Old_Weight, Name, Old_Country],
               [1.44, 69, Name, 'UK'],
               _).

test(obj_rewrite_weak3, [fail]) :-

   obj_construct(man_v,
                 [name, height, weight],
                 ['Luda', 1.40, 99], Luda1),
   obj_rewrite(Luda1, fail,
               [height, weight, name, country],
               _,
               [1.44, 69, _, 'UK'],
               _).

test(obj_rewrite_with_evals1, [X == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [X], [callup_v], _).

test(obj_rewrite_with_evals2, [fail]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [_], [citizen_v], _).

test(obj_rewrite_with_evals3, [X == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [X], [X], _).

test(obj_rewrite_with_evals4) :-
% <NB> can't reset eval field, compare with test(obj_rewrite)
   
   obj_construct(citizen_v, [sex, birthday], [man, 1994], O),
   obj_rewrite(O, [class], [_], [_], _).

test(obj_sort_parents1,
     [P4 == [http_invalid_mixed_headers_v,
             http_invalid_headers_v,
             http_request_headers_v,
             http_response_headers_v,
             http_headers_v,
             object_v, object_base_v]]
     ) :-

   obj_construct(http_request_headers_v, [], [], V1),
   obj_rebase((http_headers_v -> http_response_headers_v),
              V1, V2),
   obj_rebase((http_headers_v -> http_invalid_mixed_headers_v),
              V2, V3),

   obj_sort_parents(V3, [http_invalid_bulk_headers_v,
                         http_invalid_mixed_headers_v,
                         http_invalid_headers_v,
                         http_request_headers_v,
                         http_response_headers_v,
                         http_headers_v,
                         tarjan_vertex_v,
                         db_object_v,
                         object_v, object_base_v], V4),
   obj_parents(V4, P4),
   functor(V4, F4, _),
   assertion(F4 == http_invalid_mixed_headers_v).

test(obj_sort_parents2,
     [throws(error(insufficient_class_order(Order,
                                            Orig_Order), _))]
     ) :-

   Order = [http_invalid_bulk_headers_v,
            http_invalid_mixed_headers_v,
            http_request_headers_v,
            http_response_headers_v,
            http_headers_v,
            tarjan_vertex_v,
            db_object_v,
            object_v, object_base_v],
   
   obj_construct(http_request_headers_v, [], [], V1),
   obj_rebase((http_headers_v -> http_response_headers_v),
              V1, V2),
   obj_rebase((http_headers_v -> http_invalid_mixed_headers_v),
              V2, V3),

   obj_parents(V3, Orig_Order),
   obj_sort_parents(V3, Order, _).


test(obj_sort_parents_bug1) :-

   new_http_user(HTTP_User),
   obj_rebase((object_v -> citizen_v),
              HTTP_User, U1),

   class_parents(http_user_v, HTTP_User_Parents),
   append([citizen_v, man_v], HTTP_User_Parents,
          Parents_Order),

   obj_sort_parents(U1, Parents_Order, HTTP_Citizen),
   % citizen_v -> man_v -> http_user_v -> ...object_base_v

   obj_copy(HTTP_Citizen, HTTP_Citizen_Copy),
   obj_field(HTTP_Citizen, cookie_db_key, Key1),
   obj_field(HTTP_Citizen_Copy, cookie_db_key, Key2),
   assertion(Key1 \= Key2).

test(class_fields) :-

   class_fields(object_base_v, Object_Base_V_Fields),
   assertion(Object_Base_V_Fields == []),

   class_fields_new(object_base_v, Object_Base_V_New_Fields),
   assertion(Object_Base_V_New_Fields == []),
   
   class_fields(object_v, Object_V_Fields),
   assertion(Object_V_Fields == []),

   class_fields_new(object_v, Object_V_New_Fields),
   assertion(Object_V_New_Fields == []),
   
   class_fields(man_v, Man_V_Fields),
   assertion(Man_V_Fields == [height, name, sex, surname, weight]),

   class_fields_new(man_v, Man_V_New_Fields),
   assertion(Man_V_New_Fields == [height, name, sex, surname,
                                  weight]),
   
   class_fields(citizen_v, Citizen_V_Fields),
   assertion(Citizen_V_Fields == [birthday, country, height, id,
                                  name, sex, surname, weight]),

   class_fields_new(citizen_v, Citizen_V_New_Fields),
   assertion(Citizen_V_New_Fields == [birthday, country, id]).


% After some time it will always fails (because it depends
% on the current year, see The Uranium Book).
test(eval_fields1, [Class == callup_v]) :-

   obj_construct(citizen_v, [sex, birthday], [man, 1994], C), 
   obj_field(C, class, Class).


test(cleanup_obj_rebase, [blocked(transaction_bug), N1 =:= N2]) :-

   aggregate(count, A^B^C^(objects:class_id(A, B, C)), N1),
   catch((obj_construct(numeric_id_v, [], [], Obj1),
          obj_rebase((object_v -> citizen_v), Obj1, _)),
         E, true),
   E = error(EX, _), functor(EX, duplicate_field, _),
   aggregate(count, A^B^C^(objects:class_id(A, B, C)), N2).


test(eval_fields1) :-

   % man_v doesn't define class eval, so in this example
   % the eval from citizen_v is always used.
   
   obj_construct(citizen_v,
                 [sex, birthday], [man, 1994], C1),
   obj_field(C1, class, Cl1),
   obj_parents(C1,
               [man_v, citizen_v, object_v, object_base_v],
               C2),
   obj_field(C2, class, Cl2),
   assertion(Cl1 == callup_v),
   assertion(Cl2 == callup_v).

test(eval_fields2) :-

   obj_construct(callup_v,
                 [sex, birthday], [man, 1994], C1),
   obj_field(C1, class, Cl1),
   obj_rebase((citizen_v -> man_v), C1, C2),
   obj_parents(C2, P2),

   % for sure, citizen_v is not present
   assertion(P2 ==
            [callup_v, man_v, object_v, object_base_v]),

   obj_field(C2, class, Cl2),
   assertion(Cl1 == callup_v),
   assertion(Cl2 == man_v).

:- end_tests(v).
