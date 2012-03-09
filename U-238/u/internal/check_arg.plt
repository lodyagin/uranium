:- begin_tests(check_arg).
:- use_module(u(internal/check_arg)).


test(decode_arg1, [setup(clear_decode_arg)]) :-

   decode_arg([[weak, true], [strict, false, fail]], weak, R1,
              context(test/1, _)),
   assertion(R1 == weak),
   
   decode_arg([[weak, true], [strict, false, fail]], weak, R2,
              context(test/1, _)),
   assertion(R2 == weak),
   
   decode_arg([[weak, true], [strict, false, fail]], strict, R3,
              context(test/2, _)),
   assertion(R3 == strict),

   decode_arg([[weak, true], [strict, false, fail]], strict, R4,
              context(test/2, _)),
   assertion(R4 == strict),

   decode_arg([[weak, true], [strict, false, fail]], fail, R5,
              context(test/3, _)),
   assertion(R5 == strict),

   decode_arg([[weak, true], [strict, false, fail]], true, R6,
              context(test/3, _)),
   assertion(R6 == weak),

   decode_arg([[weak, true], [strict, _]], _, R7,
              context(test/4, _)),
   assertion(R7 == strict),

   decode_arg([[weak, true], [strict, _]], _, R8,
              context(test/4, _)),
   assertion(R8 == strict).

   
test(decode_arg2, [throws(error(domain_error(_,_), _)),
                   setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], a, _,
              context(user/0, _)).

test(decode_arg3, [fail, setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _, _).

test(decode_arg4, [fail, setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _,
              context(_/3, _)).

test(decode_arg5, [fail, setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _,
              context(a/_, _)).

test(decode_arg6, [fail, setup(clear_decode_arg)]) :-
   decode_arg([[weak, true], [strict, false, fail]], weak, _,
              context(a/3)).

test(decode_arg_bug1,
     [setup(clear_decode_arg),
      [Weak1, Weak2] == [weak, throw]
      ]
    ) :-

      decode_arg([[throw, throws, strict, s],
               [weak, _, unbound, w],
               [fail, false, f]],
              _, Weak1,
              context(decode_arg_bug/0, _)),

      decode_arg([[throw, throws, strict, s],
               [weak, _, unbound, w],
               [fail, false, f]],
              throw, Weak2,
              context(decode_arg_bug/0, _)).

test(decode_arg_bug1_2,
     [setup(clear_decode_arg),
      [Weak2, Weak1] == [throw, weak]
      ]
    ) :-

      decode_arg([[throw, throws, strict, s],
               [weak, _, unbound, w],
               [fail, false, f]],
              throw, Weak2,
              context(decode_arg_bug/0, _)),

      decode_arg([[throw, throws, strict, s],
               [weak, _, unbound, w],
               [fail, false, f]],
              _, Weak1,
              context(decode_arg_bug/0, _)).


:- end_tests(check_arg).
