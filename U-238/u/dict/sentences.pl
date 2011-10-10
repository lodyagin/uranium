:- module(sentences,
          [make_sentence/4,
           glue_sentence/3
           ]).

:- use_module(dict).

make_sentence(Words, Words, [], 0) :- !.

make_sentence([Word | Tail], Words_Out, Sentence, Length) :-
    string_length(Word, L1),
    (L1 =:= Length ->
        
        Sentence = [Word],
        Words_Out = Tail

        ;

        (L1 > Length ->

            Sentence = [],
            Words_Out = [Word | Tail]

            ;

            Sentence = [Word | S2],
            L2 is Length - L1 - 1, % space
            make_sentence(Tail, Words_Out, S2, L2)
        )
     ).

glue_sentence([], _, []) :- !.

glue_sentence([Word], _, Word) :- !.

glue_sentence([Word|Tail], Separator, Sentence) :-
        string_concat(Word, Separator, W2),
        glue_sentence(Tail, Separator, S2),
        string_concat(W2, S2, Sentence).
