:- module(dict,
          [load_random_words/2,
           format_word_list/2
           ]).

:- use_module(u(ur_math)).
:- use_module(u(ur_lists)).

n_words(96238).
dict_file_name('dict/zali_translit.txt').

load_random_words(Num, Words) :-
    n_words(Ceil),
    dict_file_name(File_Name),
    random_list(Num, Ceil, Idx_L), 
    sort(Idx_L, Idx_L_Sorted), 
    num_diff_list([-1|Idx_L_Sorted], Idx_Diff), % -1 - indexing from 0
    open(File_Name, read, Stream),
    load_lines_by_idx(Stream, Idx_L_Sorted, Idx_Diff, Indexed_Words),
    extract_by_key_order(Idx_L, Indexed_Words, OI_Words),
    maplist(dekeying, OI_Words, Words),
    close(Stream).

dekeying(_ - E, E).

format_word_list(Frmt, Words) :-
    maplist(s_format(Frmt), Words).

s_format(Frmt, W) :- format(Frmt, [W]).

load_lines_by_idx(_, _, [], []) :- !.

load_lines_by_idx(Stream, [Idx | Idx_Tail], [Step | Diff_Tail], [Idx - Line |Lines_Tail]) :-
    Skip is Step - 1,
    skip_lines(Stream, Skip),
    read_line_to_codes(Stream, Codes),
    string_to_list(Line, Codes),
    load_lines_by_idx(Stream, Idx_Tail, Diff_Tail, Lines_Tail).
    
skip_lines(_, 0) :- !.

skip_lines(Stream, N) :-
    N > 0,
    N1 is N - 1,
    read_line_to_codes(Stream, _),
    skip_lines(Stream, N1).
    
