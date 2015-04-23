:- module(class_options, []).

:- use_module(u(ur_option)).
:- use_module(library(clpfd)).

setup_options :-
   Random = [[meta_option(generator/1)], 
             [group(rand_state), 
              option(rand_state/1), option(rand_state/2)
             ],
             [group(det), option(semidet/0), option(nondet/0)],
             [group(phase), option(phase/1)]
            ],
   Weak =   [[group(how_weak), option(throw/0), option(throws/0),
              option(strict/0), option(s/0),
              option(unbound/0), option(weak/0), option(w/0),
              option(fail/0), option(false/0), option(f/0),
              default(strict)]
            ],
   ur_options(randgen:random_options,  % NB no defaults (to overwrite
                                       % possibility in
                                       % random_options/7
              Random),
   ur_options(class_diagram:class_diagram,
              [[option(is_primary/1), default(is_primary(_))],
               [option(only_class/1), default(only_class(_))]
              ]),
   ur_options(object_v:nonneg_set_gen,
              [[option(range/2), default(range(1, 100000))]
              ]),
   ur_options(http_request_v:http_method_set_gen,
              [[multi_group(http_method_type),
                option(http_method_standard/0),
                option(http_method_extension/0),
                default([http_method_standard])],
               [option(length/2), default(length(1, 100))]
              ]),
   ur_options(ur_enums:enum_integer,
              [[group(scope), option(module/1), option(field/1),
                option(global/0), default(global)],
               [group(scope_policy), 
                option(strict_scope/0), % allow return enums only from
                                        % one module
                option(weak_scope/0), % allow to return enums from all
                                      % module if value of Enum or
                                      % Integer match
                default(strict_scope)]
               | Weak]),
   ur_options(gt_strings:random_string,
              [[multi_group(length), %not_empty,
                option(empty/0), option(length/1), option(length/2),
                default([empty, length(1, 80)])],
               [multi_group(pattern),
                option(regex/1),
                option(range/1),
                meta_option(pattern/1),
                option(static/1),
                default([range(32..126)])]
               | Random
              ]),
   ur_options(gt_numbers:random_number,
              [[multi_group(domain), 
                option(integer/0), option(rational/0), option(real/0),
                default([integer])],
               [multi_group(pattern),
                option(range/1),
                meta_option(pattern/1),
                default([range(-100..100)])]
               | Random
              ]),
   ur_options(ixpath:ixpath,
              [[group(v),
                option(v/0),
                option(vx/0), option(vix/0), option(vixc/0)]]).

:- initialization setup_options.
