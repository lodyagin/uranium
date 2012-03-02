:- begin_tests(objects_i).
:- use_module(u(internal/objects_i)).

test(class_fields) :-

   class_id(Id, citizen_v), !,
   
   class_fields(_:_, Id, false, false, F1),
   assertion(F1 = [height:_, name:_, sex:_, surname:_, weight:_]),

   class_fields(_:_, Id, false, true, F2),
   assertion(F2 = [class:_, functor:_]),

   class_fields(_:_, Id, true, false, F3),
   assertion(F3 = [birthday:_, country:_, id:_]),

   class_fields(_:_, Id, true, true, F4),
   assertion(F4 = [age:_]),

   class_fields(_:_, Id, false, _, F5),
   assertion(F5 = [class:_, functor:_, height:_, name:_, sex:_,
                   surname:_, weight:_]),

   class_fields(_:_, Id, true, _, F6),
   assertion(F6 = [age:_, birthday:_, country:_, id:_]),

   class_fields(_:_, Id, _, false, F7),
   assertion(F7 = [birthday:_, country:_, height:_, id:_, name:_,
                   sex:_, surname:_, weight:_]),

   class_fields(_:_, Id, _, true, F8),
   assertion(F8 = [age:_, class:_, functor:_]),

   class_fields(_:_, Id, _, _, F9),
   assertion(F9 = [age:_, birthday:_, class:_, country:_,
                   functor:_, height:_, id:_, name:_, sex:_,
                   surname:_,weight:_]).   

                
:- end_tests(objects_i).
