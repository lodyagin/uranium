:- begin_tests(check_arg).
:- use_module(u(internal/check_arg)).


test(decode_arg1, [setup(clear_decode_arg)]) :-

   decode_arg([[weak, true], [strict, false, fail]], weak, R1,
              test),
   assertion(R1 == weak),
   
   decode_arg([[weak, true], [strict, false, fail]], weak, R2,
              test),
   assertion(R2 == weak),
   
   decode_arg([[weak, true], [strict, false, fail]], strict, R3,
              test/2),
   assertion(R3 == strict),

   decode_arg([[weak, true], [strict, false, fail]], strict, R4,
              test/2),
   assertion(R4 == strict),

   decode_arg([[weak, true], [strict, false, fail]], fail, R5,
              test/3),
   assertion(R5 == strict),

   decode_arg([[weak, true], [strict, false, fail]], true, R6,
              test/3),
   assertion(R6 == weak),

   decode_arg([[weak, true], [strict, _]], _, R7, test/4),
   assertion(R7 == strict),

   decode_arg([[weak, true], [strict, _]], _, R8, test/4),
   assertion(R8 == strict).

   
test(decode_arg2, [throws(error(domain_error(_,_), _)),
                   setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], a, _, user).

test(decode_arg3, [throws(error(instantiation_error, _)),
                   setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _, _).

test(decode_arg4, [throws(error(instantiation_error, _)),
                   setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _, _/3).

test(decode_arg5, [throws(error(instantiation_error, _)),
                   setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _, a/_).

:- end_tests(check_arg).
