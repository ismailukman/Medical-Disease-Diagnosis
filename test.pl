%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Prolog :- Medical Disease Diagnosis expert System
% Name : Ismaila Lukman Enegi
% type "help." for help
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(jpl)).
start :-sleep(0.2),
		write('-----------------------------------------------------------------'),nl,
		sleep(0.1),
		write('*****************************************************************'),nl,
		sleep(0.1),
		write("##############|||MEDICAL EXPERT SYSTEM |||##################"),nl,
		sleep(0.1),
		write('*****************************************************************'),nl,
		sleep(0.1),
		write('-----------------------------------------------------------------'),nl,nl,nl,


        /*write("Hi. How are you? First of all tell me your name Please : "),
        read(Patient),*/


interface1.


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
%New Rules for Differential Diagnosis

cannot_have_maleriation:-
       ages(infant).

can_have_maleriation :-
       ages(adult).

environment:-
       bacterial(yes).

%explain(x,y):-




hypothesis(Patient,cholera) :-
       environment,
       symptom(Patient,diarrhea),
       symptom(Patient,nausea),
       symptom(Patient,vomiting),
       symptom(Patient,dehydration).

hypothesis(Patient,maleria) :-
       cannot_have_maleriation,
       symptom(Patient,headache),
       symptom(Patient,fever),
       symptom(Patient,chills),
       symptom(Patient,diarrhea),
       symptom(Patient,frequent_sweating).

 hypothesis(Patient,maleriation) :-
       can_have_maleriation,
       symptom(Patient,headache),
       symptom(Patient,fever),
       symptom(Patient,chills),
       symptom(Patient,diarrhea),
       symptom(Patient,frequent_sweating).


hypothesis(Patient,typhoid) :-
	symptom(Patient,fever),
	symptom(Patient,poor_appetite),
	symptom(Patient,headache),
	symptom(Patient,diarrhea),
	symptom(Patient,fatigue).

hypothesis(Patient,measles) :-
       symptom(Patient,fever),
       symptom(Patient,cough),
       symptom(Patient,conjunctivitis),
       symptom(Patient,runny_nose),
       symptom(Patient,rash).

 hypothesis(Patient,dysentery) :-
	symptom(Patient,fever),
	symptom(Patient,chills),
	symptom(Patient,vomiting),
	symptom(Patient,diarrhea),
	symptom(Patient,fatigue),
        symptom(Patient,abdominal_pain).

hypothesis(Patient,common_cold) :-
        symptom(Patient,headache),
        symptom(Patient,sneezing),
        symptom(Patient,sore_throat),
        symptom(Patient,runny_nose),
        symptom(Patient,chills).

hypothesis(Patient,mumps) :-
    symptom(Patient,fever),
    symptom(Patient,swollen_glands),
	symptom(Patient,fatigue),
	symptom(Patient,headache).

hypothesis(Patient,chicken_pox) :-
    symptom(Patient,fever),
    symptom(Patient,chills),
    symptom(Patient,body_ache),
    symptom(Patient,rash).

%******
hypothesis(Patient,tuberculosis):-
	symptom(Patient, persistent_cough),
	symptom(Patient, fatigue),
	symptom(Patient, weight_loss),
	symptom(Patient, poor_appetite),
	symptom(Patient, fever),
	symptom(Patient, coughing_blood),
	symptom(Patient, frequent_sweating).

hypothesis(Patient,pneumonia):-
	symptom(Patient, cough),
	symptom(Patient, fever),
    symptom(Patient, chills),
	symptom(Patient, shortness_of_breath).

hypothesis(Patient,pertusis):-
	symptom(Patient, runny_nose),
	symptom(Patient, sneezing),
	symptom(Patient, cough),
	symptom(Patient, mild_fever).

hypothesis(Patient,pneumoconiosis):-
	symptom(Patient,chronic_cough),
	symptom(Patient, wheezing),
	symptom(Patient,shortness_of_breath).

hypothesis(Patient,sarcoidosis):-
	symptom(Patient, dry_cough),
	symptom(Patient, shortness_of_breath),
	symptom(Patient, mild_chest_pain),
	symptom(Patient, scaly_rash),
	symptom(Patient, fever),
	symptom(Patient, red_bumps_on_legs),
	symptom(Patient, sore_eyes),
	symptom(Patient, swollen_ankles).

hypothesis(Patient,asbestosis):-
	symptom(Patient, chest_tightness),
	symptom(Patient, shortness_of_breath),
	symptom(Patient, chest_pain),
	symptom(Patient, lack_of_appetite).


hypothesis(Patient,asthma):-
	symptom(Patient, wheezing),
	symptom(Patient, cough),
	symptom(Patient, chest_tightness),
	symptom(Patient, shortness_of_breath).

hypothesis(Patient,bronchiolitis):-
	symptom(Patient, wheezing),
	symptom(Patient, fever),
	symptom(Patient, blue_skin),
	symptom(Patient, rapid_breath).

hypothesis(Patient,influenza):-
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

hypothesis(_,"disease. But I'm Sorry, I don't seem to be able to diagnose the disease").

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
	sleep(0.1),
	write('Loading..'),nl,
	sleep(0.1),
	write('Loading...'),nl,
	sleep(0.1),
    nl.

:- dynamic yes/1,no/1,ages/1,bacterial/1,gender/1.

verify(P,S) :-
   (yes(S)->true ;
    (no(S)->fail ;
     ask(P,S))).

undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo :- retract(ages(_)), fail.
undo :- retract(bacterial(_)), fail.
undo :- retract(gender(_)), fail.
undo.


ptage(Age):-
       atom_number(Age,X),
       ( X >= 20 -> asserta(ages(adult)); asserta(ages(infant) )) ,
       interface01.


pt(Patient):-
       hypothesis(Patient,Disease),
      % X = format('~w \n',[Patient]),
      % Y = format('\n ~w',[Disease]),
       interface3(Patient,', you have been diagnosed with \n',Disease,'.'),
       write(Patient),write(', you have been diagnosed with '),write(Disease),write('.'),undo,end.


end :-
       nl,nl,nl,
       sleep(0.2),
       write('*****************************************************************'),nl,
       sleep(0.2),
       write("################||| THANK YOU |||#####################"),nl,
       sleep(0.2),
       write('*****************************************************************'),nl.


%**********************************
interface4:-
        jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [450,200], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),

        jpl_new(array(class([java,lang],['String'])),['Rural','Urban'], ArrayRef),
		jpl_get(ArrayRef,0,ArrayPosRef),
		jpl_get('javax.swing.JOptionPane', 'YES_NO_OPTION', YesNoRef),
		jpl_get('javax.swing.JOptionPane', 'QUESTION_MESSAGE', QuestionRef),
		jpl_call('javax.swing.JOptionPane', 'showOptionDialog', [@('null'),
					       'Environment type?\n',
					       'Question',
					       YesNoRef,
					       QuestionRef,
					       @(null),
					       ArrayRef,
					       ArrayPosRef],
					       RetVal),
	 jpl_call(F, dispose, [], _), write('Environment type? ') ,
	 ( (RetVal==0)->write(': Rural'); write(': Urban')),nl,
        ( (RetVal==0)->assert(bacterial(yes));assert(bacterial(no)) ),
        interface2.


%***************

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [450,200], _),
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
					       RetVal),
	 jpl_call(F, dispose, [], _),
	 ( (RetVal==0)->write(': Yes'); write(': No')),nl,
        ( (RetVal==0)->assert(yes(Z));assert(no(Z)), fail).


%***********
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
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Welcome.\nHow old are you? '], N),
	jpl_call(F, dispose, [], _),
	%write(N),nl,
       % ( (N == yes ; N == y) ->
      % assert(yes(Z)) ;
      % assert(no(Z)), fail).
       (N == @(null)-> write('you cancelled'),interface3('you cancelled. ','Thank you ','for using ','our Expert System.'),end,fail;
       write("Welcome.\nHow old are you: "),write(N),nl, ptage(N)).

%***********
interface01:-
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [450,200], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),

        jpl_new(array(class([java,lang],['String'])),['Male','Female'], ArrayRef),
		jpl_get(ArrayRef,0,ArrayPosRef),
		jpl_get('javax.swing.JOptionPane', 'YES_NO_OPTION', YesNoRef),
		jpl_get('javax.swing.JOptionPane', 'QUESTION_MESSAGE', QuestionRef),
		jpl_call('javax.swing.JOptionPane', 'showOptionDialog', [@('null'),
					       'Gender ?\n',
					       'Question',
					       YesNoRef,
					       QuestionRef,
					       @(null),
					       ArrayRef,
					       ArrayPosRef],
					       RetVal),
	 jpl_call(F, dispose, [], _), write('Gender? ') ,
	 ( (RetVal==0)->write(': Male'); write(': Female')),nl,
        ( (RetVal==0)->assert(gender(male));assert(gender(female)) ),
        interface4.



interface2 :-
	jpl_new('javax.swing.JFrame', ['Medical Expert System'], F),
	jpl_new('javax.swing.JLabel',['--- IMPORTANCE USER DATA COMPLETENESS IN DIFFERENCIAL DIAGNOSIS ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [500,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Please type-in your name'], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(null)
		->	write('you cancelled'),interface3('you cancelled. ','Thank you ','for using ','our Expert System.'),end,fail
		;	format("Please type-in your name : "),write(N),nl,pt(N)
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

why(Patient,X) :-
       write("Because, you have all the syptome for the disease"),
       symptom(Patient,X).





































