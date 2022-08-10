%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%To Start the system type start.
%Prolog :- Medical expert System
% Name : Ismaila Lukman Enegi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(jpl)).
start :-sleep(0.4),
		write('-----------------------------------------------------------------'),nl,
		sleep(0.2),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("##############|||MEDICAL EXPERT SYSTEM |||##################"),nl,
		sleep(0.2),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write('-----------------------------------------------------------------'),nl,nl,nl,


        /*write("Hi. How are you? First of all tell me your name Please : "),
        read(Patient),*/

interface2.

       /* hypothesis(Patient,Disease),
        write(Patient),write(', you '), write(' probably have '),write(Disease),write('.'),undo,
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.*/
%************************
%New Rules for Differential Diagnosis
%





%diarrhea
    symptom(Patient,diarrhea) :- verify(Patient," have a diarrhea (y/n) ?").
    symptom(Patient,fever) :- verify(Patient," have a fever (y/n) ?").
    symptom(Patient,rash) :- verify(Patient," have a rash (y/n) ?").
    symptom(Patient,headache) :- verify(Patient," have a headache (y/n) ?").
    symptom(Patient,runny_nose) :- verify(Patient," have a runny_nose (y/n) ?").
    symptom(Patient,conjunctivitis) :- verify(Patient," have a conjunctivitis (y/n) ?").
    symptom(Patient,cough) :- verify(Patient," have a cough (y/n) ?").
    symptom(Patient,body_ache) :- verify(Patient," have a body_ache (y/n) ?").
    symptom(Patient,chills) :- verify(Patient," have a chills (y/n) ?").
    symptom(Patient,sore_throat) :- verify(Patient," have a sore_throat (y/n) ?").
    symptom(Patient,sneezing) :- verify(Patient," have a sneezing (y/n) ?").
    symptom(Patient,swollen_glands) :- verify(Patient," have a swollen_glands (y/n) ?").
    symptom(Patient,nausea) :- verify(Patient," have nausea (y/n) ?").
    symptom(Patient,vomiting) :- verify(Patient," have vomiting (y/n) ?").
    symptom(Patient,dehydration) :- verify(Patient," have dehydration (y/n) ?").
    symptom(Patient,frequent_sweating) :- verify(Patient," have frequent_sweating (y/n) ?").
    symptom(Patient,poor_appetite) :- verify(Patient," have poor_appetite (y/n) ?").
    symptom(Patient,constipation) :- verify(Patient," have constipation (y/n) ?").
    symptom(Patient,fatigue) :- verify(Patient," have fatigue (y/n) ?").
    symptom(Patient,persistent_cough) :- verify(Patient," have persistent_cough (y/n) ?").
    symptom(Patient,weight_loss) :- verify(Patient," have weight_loss (y/n) ?").
    symptom(Patient,coughing_blood) :- verify(Patient," have coughing_blood (y/n) ?").
    symptom(Patient,chest_tightness) :- verify(Patient," have chest_tightness (y/n) ?").
    symptom(Patient,shortness_of_breath) :- verify(Patient," have shortness_of_breath (y/n) ?").
    symptom(Patient,wheezing) :- verify(Patient," have wheezing (y/n) ?").
    symptom(Patient,mild_fever) :- verify(Patient," have mild_fever (y/n) ?").
    symptom(Patient,chronic_cough) :- verify(Patient," have chronic_cough (y/n) ?").
    symptom(Patient,dry_cough) :- verify(Patient," have dry_cough (y/n) ?").
    symptom(Patient,mild_chest_pain) :- verify(Patient," have mild_chest_pain (y/n) ?").
    symptom(Patient,scaly_rash) :- verify(Patient," have scaly_rash (y/n) ?").
    symptom(Patient,red_bumps_on_legs) :- verify(Patient," have red_bumps_on_legs (y/n) ?").
    symptom(Patient,sore_eyes) :- verify(Patient," have sore_eyes (y/n) ?").
    symptom(Patient,swollen_ankles) :- verify(Patient," have swollen_ankles (y/n) ?").
    symptom(Patient,chest_pain) :- verify(Patient," have chest_pain (y/n) ?").
    symptom(Patient,blue_skin) :- verify(Patient," have blue_skin (y/n) ?").
    symptom(Patient,rapid_breath) :- verify(Patient," have rapid_breath (y/n) ?").
    symptom(Patient,nasal_congestion) :- verify(Patient," have nasal_congestion (y/n) ?").
    symptom(Patient,hoarseness) :- verify(Patient," have hoarseness (y/n) ?").

	/*symptom(_,"Sorry, I don't seem to be able to diagnose the disease.").*/

%hypothesises
%
%age('20').
%gender(male).
%gender(female).
%age(grt20).
%age(les20).
%



can_have_maleriation :-
       age('20'),
       gender(male).

cannot_have_maleriation:-
       age('18'),
       gender(female).

hypothesis(Patient,cholera, Age) :-
       %eligible(Age),
       symptom(Patient,diarrhea),
       symptom(Patient,nausea),
       symptom(Patient,vomiting),
       symptom(Patient,dehydration).

hypothesis(Patient,maleria, Age) :-
      % gender(male),age(les20),
       %cannot_have_maleriation,
      %eligible(Age),
       symptom(Patient,headache),
       symptom(Patient,fever),
       symptom(Patient,chills),
       symptom(Patient,diarrhea),
       symptom(Patient,frequent_sweating).

 hypothesis(Patient,maleriation, Age) :-
       %gender(male),age(grt20),
       %eligible(Age),
       %can_have_maleriation,
       symptom(Patient,headache),
       symptom(Patient,fever),
       symptom(Patient,chills),
       symptom(Patient,diarrhea),
       symptom(Patient,frequent_sweating).


hypothesis(Patient,typhoid, Age) :-
	symptom(Patient,fever),
	symptom(Patient,poor_appetite),
	symptom(Patient,headache),
	symptom(Patient,diarrhea),
	symptom(Patient,fatigue).

hypothesis(Patient,measles, Age) :-
       symptom(Patient,fever),
       symptom(Patient,cough),
       symptom(Patient,conjunctivitis),
       symptom(Patient,runny_nose),
       symptom(Patient,rash).

 hypothesis(Patient,dysentery, Age) :-
	symptom(Patient,fever),
	symptom(Patient,chills),
	symptom(Patient,vomiting),
	symptom(Patient,diarrhea),
	symptom(Patient,fatigue),
        symptom(Patient,abdominal_pain).

hypothesis(Patient,common_cold, Age) :-
        symptom(Patient,headache),
        symptom(Patient,sneezing),
        symptom(Patient,sore_throat),
        symptom(Patient,runny_nose),
        symptom(Patient,chills).

hypothesis(Patient,mumps, Age) :-
    symptom(Patient,fever),
    symptom(Patient,swollen_glands),
	symptom(Patient,fatigue),
	symptom(Patient,headache).

hypothesis(Patient,chicken_pox, Age) :-
    symptom(Patient,fever),
    symptom(Patient,chills),
    symptom(Patient,body_ache),
    symptom(Patient,rash).

%******
hypothesis(Patient,tuberculosis, Age):-
	symptom(Patient, persistent_cough),
	symptom(Patient, fatigue),
	symptom(Patient, weight_loss),
	symptom(Patient, poor_appetite),
	symptom(Patient, fever),
	symptom(Patient, coughing_blood),
	symptom(Patient, frequent_sweating).

hypothesis(Patient,pneumonia, Age):-
	symptom(Patient, cough),
	symptom(Patient, fever),
    symptom(Patient, chills),
	symptom(Patient, shortness_of_breath).

hypothesis(Patient,pertusis, Age):-
	symptom(Patient, runny_nose),
	symptom(Patient, sneezing),
	symptom(Patient, cough),
	symptom(Patient, mild_fever).

hypothesis(Patient,pneumoconiosis, Age):-
	symptom(Patient,chronic_cough),
	symptom(Patient, wheezing),
	symptom(Patient,shortness_of_breath).

hypothesis(Patient,sarcoidosis, Age):-
	symptom(Patient, dry_cough),
	symptom(Patient, shortness_of_breath),
	symptom(Patient, mild_chest_pain),
	symptom(Patient, scaly_rash),
	symptom(Patient, fever),
	symptom(Patient, red_bumps_on_legs),
	symptom(Patient, sore_eyes),
	symptom(Patient, swollen_ankles).

hypothesis(Patient,asbestosis, Age):-
	symptom(Patient, chest_tightness),
	symptom(Patient, shortness_of_breath),
	symptom(Patient, chest_pain),
	symptom(Patient, lack_of_appetite).


hypothesis(Patient,asthma, Age):-
	symptom(Patient, wheezing),
	symptom(Patient, cough),
	symptom(Patient, chest_tightness),
	symptom(Patient, shortness_of_breath).

hypothesis(Patient,bronchiolitis, Age):-
	symptom(Patient, wheezing),
	symptom(Patient, fever),
	symptom(Patient, blue_skin),
	symptom(Patient, rapid_breath).

hypothesis(Patient,influenza, Age):-
	symptom(Patient, headache),
	symptom(Patient, fever),
	symptom(Patient, chills),
	symptom(Patient, cough),
	symptom(Patient,conjunctivitis),
	symptom(Patient, nasal_congestion),
	symptom(Patient, runny_nose),
	symptom(Patient, sore_throat).

/*hypothesis(Patient,lung_cancer):-
	symptom(Patient, cough),
	symptom(Patient, fever),
	symptom(Patient, hoarseness),
	symptom(Patient, chest_pain),
	symptom(Patient, wheezing),
	symptom(Patient, weight_loss),
	symptom(Patient, lack_of_appetite),
	symptom(Patient, coughing_blood),
	symptom(Patient, headache),
	symptom(Patient, shortness_of_breath).
	*/

hypothesis(_,"disease. But I'm Sorry, I don't seem to be able to diagnose the disease", Age).

response(Reply) :-
        read(Reply),
        write(Reply),nl.

ask(Patient,Question) :-
	write(Patient),write(', do you'),write(Question),
	/*read(N),
	( (N == yes ; N == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail),*/

interface(', do you',Patient,Question),
	write('Loading.'),nl,
	sleep(0.2),
	write('Loading..'),nl,
	sleep(0.2),
	write('Loading...'),nl,
	sleep(0.2),
    nl.

:- dynamic yes/1,no/1.

verify(P,S) :-
   (yes(S)->true ;
    (no(S)->fail ;
     ask(P,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.

%****
%
age(adult).
eligible(X):-
   ( X >= 20 -> accert
   write('You can have maleria');
   write('You cannot have maleria') ).


ptage(Age):-

       %name(Age, Age),
      % numericValue(Age),
       %is_integer(Age),
      % string(Age),
      % integer(Age),
       %Patient(Age),
       atom_number(Age,X),
       Y is X + 1,
       eligible(Y),
       write(Y).
      %interface2(Age).


pt(Patient,Age):-
       hypothesis(Patient,Disease,Age),
       interface3(Patient,', you probably have ',Disease,'.'),
       write(Patient),write(', you probably have '),write(Disease),write('.'),undo,end.


end :-
       nl,nl,nl,
       sleep(0.7),
       write('*****************************************************************'),nl,
       sleep(0.4),
       write("################||| THANK YOU |||#####################"),nl,
       sleep(0.4),
       write('*****************************************************************'),nl.

%interface5(X,Y,Z) :-
%	atom_concat(Y,X, FAtom),
%	atom_concat(FAtom,Z,FinalAtom),
%	jpl_new('javax.swing.JFrame', ['Medical Expert System'],% F),
%       jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA
%       COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
%	jpl_new('javax.swing.JPanel',[],Pan),
%	jpl_call(Pan,add,[LBL],_),
%	jpl_call(F,add,[Pan],_),
%	jpl_call(F, setLocation, [400,300], _),
%	jpl_call(F, setSize, [500,300], _),
%	jpl_call(F, setVisible, [@(true)], _),
%	jpl_call(F, toFront, [], _),
%       jpl_call('javax.swing.JOptionPane', showInputDialog,
%       [F,FinalAtom], N),
%	jpl_call(F, dispose, [], _),
%	write(N),nl,
%	( (N == yes ; N == y)
 %     -> assert(yes(Z)) ;assert(no(Z)), fail).


interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),

        jpl_new(array(class([java,lang],['String'])),['yes','no'], ArrayRef),
		jpl_get(ArrayRef,0,ArrayPosRef),
		jpl_get('javax.swing.JOptionPane', 'YES_NO_OPTION', YesNoRef),
		jpl_get('javax.swing.JOptionPane', 'QUESTION_MESSAGE', QuestionRef),
		jpl_call('javax.swing.JOptionPane', 'showOptionDialog', [@('null'),
					       FinalAtom,
					       'Question',
					       YesNoRef,
					       QuestionRef,
					       @(null),
					       ArrayRef,
					       ArrayPosRef],
					       RetVal),write(RetVal),%write(Response),
	 jpl_call(F, dispose, [], _),
	write(RetVal),nl,
        ( (RetVal==0)->assert(yes(Z));assert(no(Z)), fail).
	%( (N == yes ; N == y)->assert(yes(Z)) ; assert(no(Z)), fail).

%interpret(1,no).  % ASCII 89  = 'Y'
%interpret(0,yes). % ASCII 121 = 'y'


%interface6(X,Y,Z) :-
%	atom_concat(Y,X, FAtom),
%	atom_concat(FAtom,Z,FinalAtom),
%        show_yes_no_dialog( FinalAtom).
%
%show_yes_no_dialog(Tekst) :-
%               jpl_new(array(class([java,lang],['String'])),['y%es','no'],
%               ArrayRef),
%		jpl_get(ArrayRef,0,ArrayPosRef),
%               jpl_get('javax.swing.JOptionPane', 'YES_NO_OPTION',
%               YesNoRef),
%               jpl_get('javax.swing.JOptionPane', 'QUESTION_MESSAGE',
%               QuestionRef),
%               jpl_call('javax.swing.JOptionPane', 'showOptionDialog',
%               [@('null'),
%					       Tekst,
%					       'Question',
%					       YesNoRef,
%					       QuestionRef,
%					       @(null),
%					       ArrayRef,
%					       ArrayPosRef],
%                                              RetVal),write(RetVal),%write(Response),
%
%
%    ( (RetVal==0)->as%sert(yes(Z));assert(no(Z)), fail).

	%interpret(RetVal,Response).



interface1 :-
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'how old are you? '],  N),
	jpl_call(F, dispose, [], _),
	%write(N),nl,
       % ( (N == yes ; N == y) ->
      % assert(yes(Z)) ;
      % assert(no(Z)), fail).
       (	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for using ','me.'),end,fail
		;	write("Hi. how old are you: "), write(N),nl, ptage(N)).



          %jpl_call('java.lang.Integer.parseInt(N)

interface2(Age) :-
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hi. How are you? First of all tell me your name please'], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for using ','me.'),end,fail
		;	write("Hi. How are you? First of all tell me your name please : "),write(N),nl,pt(N,Age)
	).


interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).

help :- write("To start the expert system please type 'start.' and press Enter key").
