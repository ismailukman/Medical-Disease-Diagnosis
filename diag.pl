%Disease Identification Game
%Start with ?- go.

go:- hypothesis(Disease),
   % write("I believe you have: ' Disease),
    write("I believe you have ",Disease,"."),nl.
    %write(Disease),
    nl,
    undo.

%Hypothesis that should be tested
hypothesis(Cold):- cold, !.
hypothesis(Flu):- flu, !.
hypothesis(Ebola):- ebola, !.
hypothesis(Measles):- measles, !.
hypothesis(unknown). /* no diagnosis*/

%Hypothesis Identification Rules
cold :-
       verify(headache),
       verify(runny_nose),
       verify(sneezing),
       verify(sore_throat).
flu :-
       verify(fever),
       verify(headache),
       verify(chills),
       verify(body_ache).
ebola :-
       verify(headache),
       verify(rash),
       verify(nausea),
       verify(bleeding).
measles :-
       verify(fever),
       verify(runny_nose),
       verify(rash),
       verify(conjunctivitis).

/* how to ask questions */
ask(Question) :-
    write('Does the patient have the following symptom: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :- (yes(S) -> true ;
               (no(S)  -> fail ;
               ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.
