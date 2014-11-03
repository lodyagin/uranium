:- module(class_options, []).

:- use_module(u(ur_option)).
:- use_module(library(clpfd)).

setup_options :-

   ur_options(class_diagram:class_diagram,
              [[option(is_primary/1), default(is_primary(_))]]),
   ur_options(object_v:nonneg_set_gen,
              [[option(range/2), default(range(1, 100000))],
               [meta_option(generator/1),
                default(generator(randgen:fd_random(lcq, gnu)))],
               [option(seed/1), default(seed(-1))]
              ]),
   ur_options(http_request_v:http_method_set_gen,
              [[multi_group(http_method_type),
                option(http_method_standard/0),
                option(http_method_extension/0),
                default([http_method_standard])],
               [option(length/2), default(length(1, 100))],
               [meta_option(generator/1),
                default(generator(randgen:fd_random(lcq, gnu)))],
               [option(seed/1), default(seed(-1))]
              ]),
   ur_options(gt_strings:random_string,
              [[multi_group(length), %not_empty,
                option(empty/0), option(length/1), option(length/2),
                default([empty, length(1, 80)])],
               [multi_group(pattern),
                option(regex/1),
                option(range/1),
                meta_option(pattern/1),
                default([range(32..126)])],
               [meta_option(generator/1),
                default(generator(randgen:fd_random(lcq, gnu)))],
               [option(seed/1), default(seed(-1))]
              ]),
   ur_options(ixpath:ixpath,
              [[group(v),
                option(v/0),
                option(vx/0), option(vix/0), option(vixc/0)]]).


:- initialization setup_options.
