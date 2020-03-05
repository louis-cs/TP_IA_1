%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de façon synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

    structure Pf : [[F,H,G],U] avec G distance parcourue, H heuristique (distance restante), F sommme des deux
    structure Pu : [U; [F,H,G], Père, A] avec A déplacement du trou

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
	% initialisations Pf, Pu et Q 

	% lancement de Aetoile

initial_state(I),
final_state(Fin), 
heuristique2(I,Fin,H0),
empty(Pf0),
empty(Pu0),
empty(Q),
insert(([H0,H0,0],I),Pf0,Pf), 
insert((I,[H0,H0,0],nil,nil),Pu0,Pu),
aetoile(Pf,Pu,Q).


%*******************************************************************************

aetoile([],[],Q) :- 
    print("Pas de solutions").  

    
% Sf == ([F,H,G],S) ---------- Su == (S,[F,H,G],Pere, A) 

% aetoile( [ Sf | Tf ], [ Su | Tu ], Qs) :-
aetoile( Pf, Pu, Qs) :-

    ( (suppress_min([[F,H,G], U],Pf,New_Pf), final_state(U)) ->
        print("solution trouvée")
    ;
    print(S),
    print(\n), 
    suppress_min(S2,Pu,New_Pu) ,
    print(S2),    
    ( belongs(Sf,Qs) -> 
        aetoile(Tf,Tu,Qs)
    ;

        nth0(0,Sf,Heuris),
        nt0(2,Heuris,G),
/* trouver le G à donner à notre expand /* 

        expand(Sf,SuccessorList,G),
        loop_successors(Sf,SuccessorList,Qs,Pu,Pf),
        aetoile(Tf,Tu,Qs)
).

expand(Sf,SuccessorList,G) :-

    final_state(Fin),

/*
    alors arnaud a décidé d'aller vite:
    On fait un find all ou l'on veut sortir une listes d'éléments comme dans la liste Pu. 
    On prend une direction, on applique la règle, on applique l'heuristique sur l'état trouvé, 
    On calcule les valeurs de H et de G. 
    On renvoit l'élément avec les nouvelles heuristiques.  
*/
    
    findall([Next_State,[NewF,NewH,NewG],Sf,A], (member(A,[up, down, left, right]), rule(A,1,Sf,Next_State), heuristique2(Next_State,Fin,NewH),NewG is G+1, NewF is NewG+NewH), SuccessorList).



%[State,[F,H,G],Father,A] == Puccessor 

loop_successors(Sf,[ Puccessor| OtherPuccessor], Qs,Pu,Pf) :-

/* Belongs peut etre à changer !!*/ 

    (belongs(Puccessor,Qs) ->
        loop_successors(Sf,OtherPuccessor,Qs,Pu,Pf)
    ;
        (belongs(Puccessor,Pu) ->
            nth0(1,Puccessor,Heuris_suiv),
            nth0(0,Heuris_suiv,Fsuiv),
            nth0(1,Sf,Heuris_act),
            nth0(0,Heuris_act,F),
/* On isole les deux valeurs à comparer /* 

            ((Fsuiv@<F) -> 
                suppress( Sf,Pu,NewPu),
                insert(Puccessor,NewPu,UpdatedPu),
                nth0(0,Sf,OldU),
                nth0(0,Puccessor,NewU),
                print(138), 
                suppress([Heuris_act,OldU],Pf,NewPf), 
                insert([Heuris_suiv,NewU],NewPf,UpdatedPf),
                insert(Puccessor,Qs,UpdatedQs),
            ).
        ;
            insert(Puccessor,Pu,UpdatedPu),
            nth0(0,Puccessor,NewU),
            insert([Heuris_suiv,NewU],Pf,UpdatedPf)
        ).,
          loop_successors(Sf,OtherPuccessor,Qs,Pu,Pf)
    ).

    
	
*/
	
   
