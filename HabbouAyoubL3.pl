entrerC:-write("saisir coefficient:"),nl,
		read(X),nl,
		write("saisir puissance:"),nl,
		read(X1),nl,
		remplir([[X,X1]]).

/*tester la forme de la liste*/
forme([]).
forme([[X,X1]|L]):-forme([L]).

/*Pour la derniere composante*/
remplir([[X,X1]]) :-affiche(X,X1).

/*toute la liste*/
remplir([[X,X1]|L]):-affiche(X,X1),remplir(L),!.

/* coefficient =0*/ 
affiche(0,X) :-!,write(''). 

/*puissance =0*/ 
affiche(X,0) :- !,write(X). 

/*puissance =1*/ 
affiche(X,1) :- !,write(' + '), write(X),write('x'). 

/*puissance >1*/ 
affiche(X,P) :-write(' + '),write(X),write('x^'),write(P).

/*----------------------------------------------------*/
/*somme*/ 
som([[Z,Y]|L],[],[[Z,Y]|L]).

som([],[[Z,Y]|L],[[Z,Y]|L]).

som([[X,Y]],[[X1,Y]],[[Z,Y]]):-Z is X+X1,!.

som([[X,Y]|L],[[X1,Y]|L1],[[Z,Y]|R]):-Z is X+X1,som(L,L1,R),!.

som([[X,Y]|L],[[X1,Y1]|L1],[[X,Y]|R]):-som(L,[[X1,Y1]|L1],R),!.

/*soustraction*/ 
sous([[Z,Y]|L],[],[[Z,Y]|L]).

sous([],[[Z,Y]|L],[[Z1,Y]|L]):-Z1 is 0-Z.

sous([[X,Y]],[[X1,Y]],[[Z,Y]]):-Z is X-X1,!.

sous([[X,Y]|L],[[X1,Y]|L1],[[Z,Y]|R]):-Z is X-X1,sous(L,L1,R),!.

sous([[X,Y]|L],[[X1,Y1]|L1],[[X,Y]|R]):-sous(L,[[X1,Y1]|L1],R),!.

/*produit*/ 
pro(_,[],[]):-!.
pro([],_,[]):-!.
pro([[X,Y]],[[X1,Y1]|L1],[[Z,A]|R]):-Z is X*X1,A is Y+Y1,pro([[X,Y]],L1,R),!.

/*derivé*/
der([],[]):-!.
der([[X,Y]],[[Z,C]]):-C is Y-1,Z is X*Y,!.
der([[X,Y]|L],[[Z,C]|R]):-C is Y-1,Z is X*Y,der(L,R),!.

/*evaluation*/
evalu([],VAL,0).
evalu([[X,P]|L1],VAL,Y):-evalu(L1,VAL,Y1), Y is (X*(VAL**P))+Y1. 

/*simplifier*/
simp([],_):-!.
simp([[C,D]|L1],L2):-simp1(L1,[C,D],L3),simp(L3,L2),!.
simp([[C,D]|L1],[[C,D]|L2]):-not(simp1(L1,[C,D],L3)),simp(L1,L2).

simp1([],[C1,D1],_):-!,fail.
simp1([[C1,D]|L1],[C2,D],[[C,D]|L1]):-!, C is C1+C2.
simp1([[C,D]|L1],[C1,D1],[[C,D]|L2]):- C1\=C , D1\=D , simp1(L1,[C1,D1],L2).