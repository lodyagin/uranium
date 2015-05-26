:- module(gt_db,
          [db_copy_random/4  % +SrcDB, +DstDB, +Options0, -Options
          ]).

:- use_module(u(rand/randgen)).
:- use_module(u(ur_terms)).
:- use_module(u(internal/check_arg)).

:- meta_predicate db_copy_random(+, +, :, -).


%% db_copy_random(+SrcDB, +DstDB, +Options0, -Options) is det.
%
% Randomly selects objects from SrcDB and copy to DstDB.
% It is DB version of random_sublist/4.
%
db_copy_random(SrcDB, DstDB, OM:Options0, Options) :-
   Ctx = context(db_copy_random/4, _),\
   check_db_key(SrcDB),
   check_db_key(DstDB),
   (  nonvar(Options), Options = OM:Options2
   -> true
   ;  Options = Options2
   ),
   options_to_object(db_copy_random, OM:Options0, Options1),
   (  random_options(Options1, Options2, Det, Generator, Seed0,
                     Seed, phase_match)
   -> length(List, FullLength),
      obj_field(Options2, length, Lengths),
      random_member(length(LengthDom0), Lengths, 
                    Det, Generator, Seed0, Seed1),
      replace_subterms([gt_lists:max_subst(FullLength), calculate], 
                       LengthDom0, LengthDom),
      N in LengthDom,
      fd_random(Generator, Seed1, Seed2, N),
      db_copy_random_int(SrcDB, DstDB, N, Generator, Seed2, Seed),
      (  Det == semidet -> ! ; true )
   ;  Options2 = Options1
   ).
