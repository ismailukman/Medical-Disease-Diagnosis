loves(romeo,juliet).
loves(juliet, romeo) :- loves(romeo, juleit).

happy(albert).
happy(alice).
happy(bob).
happy(bill).
with_albert(alice).
go_to_party_with_alice(albert).

runs(albert) :-
    happy(albert).

dances(alice) :- happy(alice),
      happy(alice),
      with_albert(alice).

smile(albert):-
     with_albert(alice),
     go_to_party_with_alice(albert).

when_are_they_in_party :-
         dances(alice),
         smile(albert),
         write('When everyone is happy and albert smiles').


stabs(tybalt,mercutio, sword).
hates(romeo, X):-
      stabs(X, mercutio, sword).


what_grade(5):-
    write('Go to kindergartin').

what_grade(6):-
    write('Go to 1st Grade').

what_grade(Other):-
    Grade is Other - 5,
    format('Go to grate ~w', [Grade]).

has(albert, olive).

owns(alber, pet(cat, olive)).

customer(tom, smith, 20.55).

customer(sally, smith, 120.55).

get_cut_bal(FName, LName):-
    customer(FName, LName, Bal),
    write(FName), tab(1),
    format('~w owes us $~2f ~n', [LName, Bal]).
is_even(X):-
    Y is X//2,
    X=:=2*Y.

say_hi :-
    write('What is your name? '),
    read(A),
    write('What is your age? '),
    read(B),
    write('Hello '),
    write(A),nl,
    write('You are ').
    %format('Hi ~$', X).

count_to_10(10):-
    write(10),nl.
count_to_10(X):-
    write(X ),
    Y is X + 1,
    count_to_10(Y).
count_down(Low,High):-
    between(Low, High, Y),
    Z is High -Y,
    write(Z), nl.
count_dwn(Low,High):-
    Z is High - 1,
    High is Z,
    write(Z), nl,
    count_dwn(Low, High).

guess_num:-
    loop(start).
loop(15):-
    write('you guessed it').

loop(X):-
    X \= 15,
    write('Guess Number '),
    read(Guess),
    write(Guess),
    write(' is not the number '), nl,
    loop(Guess).
write_list([]).

write_list([Head|Tail]):-
    Head1 is Head *2,
    write(Head1), nl,
    write_list(Tail).

join_str(Str1, Str2,Str3):-
    name(Str1,StrList1),
    name(Str2, StrList2),
    append(StrList1,StrList2, StrList3),
    name(Str3, StrList3).

position('Lukman',driver).
position('Ahmed', lecturer).

find_position:-
    write('whose position do you wish to know? '),
    read(Pos),nl,
    position(Pos,Output),nl,
    write(Output).

%**********************


order(tubenose) :-
    nostrils(external_tubular),
    live(at_sea),
    bill(hooked).

order(waterfowl) :-
    feet(webbed),
    bill(flat).

family(albatross) :-
    order(tubenose),
    size(large),
    wings(long_narrow).

family(swan) :-
    order(waterfowl),
    neck(long),
    color(white),
    flight(ponderous).


bird(laysan_albatross):-
    family(albatross),
    color(white).

bird(black_footed_albatross):-
    family(albatross),
    color(dark).

bird(whistling_swan) :-
    family(swan),
    voice(muffled_musical_whistle).

bird(trumpeter_swan) :-
    family(swan),
    voice(loud_trumpeting).

bird(mallard):-
    family(duck),
    voice(quack),
    head(green).

bird(mallard):-
    family(duck),
    voice(quack),
    color(mottled_brown).

eats(X):- ask(eats, X).

feet(X):- ask(feet, X).

wings(X):- ask(wings, X).

neck(X):- ask(neck, X).

color(X):- ask(color, X).


ask(Attr, Val):-
    write(Attr:Val),
    write('? '),
    read(yes).

 atom_number('3').
string_num(X):-
    X1 = string_number(X),
    write(X1).

myMaths:-
    write('Enter first number '),
    readreal(Num1),
    write('Enter second number '),
    readint(Num2),
 atom_number(Num2,X),
% atom_number(Num1,Y),
%( H1|Num1), (H2|Num2),

   % ( str_int(Num1,Numa), str_int(Num2,Numb),  write(Numa),nl);
    Ans is Num1 + Num2,
    write('The sum is '),tab(3),write(Ans).
%atom_number('20',X)
