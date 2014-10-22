:- module(clpfd_regex,
          [range_drep/2
           ]).

range_drep(Regex, Drep) :-

  true.

range(Drep) -->

  [From, '-', To],
  ...