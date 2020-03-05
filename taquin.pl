/*:- lib(listut).       % Placer cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)

      */                
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)
   

initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5



% AUTRES EXEMPLES POUR LES TESTS DE  A*


/*initial_state([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2

initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10
			
initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
			
initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 

initial_state([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)
*/  


   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   
final_state([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).

final_state4x4([[1, 2, 3, 4],
             [5, 6, 7, 8],
             [9, 10, 11, 12],
             [13, 14, 15, vide]]).
			 
   %********************
   % AFFICHAGE DUN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal            
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait dune occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.

   
   %***********************************************************************
   % Sélection dun élément dans une position et renvoi des coordonnées
   % OU Clarification de lélément en fonction des coordonnées données 
   %***********************************************************************
   
select_elem(Mat,Elem,C,L) :-
    nth1(L,Mat,Ligne), 
    nth1(C,Ligne,Elem).

bien_place(Elem) :-
    initial_state(Ini),
    final_state(Fin),
    select_elem(Ini,Elem,C,L),
    select_elem(Fin,Elem,C,L).

mal_place(Mat, Elem) :- 
    final_state(Fin),
    select_elem(Mat,Elem,C1,L1),
    select_elem(Fin,Elem,C2,L2),
    [C1,L1] \= [C2,L2],
    Elem \= vide.
   
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) dune piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	
	coordonnees([L,C], Mat, Elt) :-
        select_elem(Mat,Elt,C,L).

   %*************
   % DISTANCE DE MANHATTAN
   %*************

    /*manhattan_once([L1,C1],[L2,C2],Dist) :- 
        V1 is abs(L1-L2),
        V2 is abs(C1-C2),
        sumlist([V1,V2],Dist).

    aux([],Mat,0).

    aux([H|T], Mat, Dist) :-
        T \= [], 
        final_state(Fin), 
        coordonnees([L2,C2],Fin,H),
        coordonnees([L,C],Mat,H),
        manhattan_once([L,C],[L2,C2],Mem),
        manhattanlist(T, Mat, Dist+Mem).(Ini)*/
    manhattan(Elem,M1,M2,D) :- 
        coordonnees([L1,C1],M1,Elem),
        coordonnees([L2,C2],M2,Elem),
        sumlist([abs(L1-L2),abs(C1-C2)],D).		

    						 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
    heuristique1(U, H).  % au debut on utilise l'heuristique 1 
%   heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2  
   
   
   %****************
   %HEURISTIQUE no 1
   %****************
   % Nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F

   
heuristique1(U, H) :- 
    findall(Elem,mal_place(U,Elem),SuperList),
    length(SuperList,H).
   
   
   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme des distances de Manhattan à parcourir par chaque piece
   % entre sa position courante et sa positon dans letat final

heuristique2(U,Fin,Dist) :-
    flatten(U,Mylist),     
    aux(Mylist,U,Fin, Dist).

aux([], State, Fin, 0). 

aux([H|T],State,Fin, Tot) :-

    aux(T,State,Fin, Dist),
    manhattan(H, State, Fin, Mem),
    Tot is Dist+Mem.





    
							
