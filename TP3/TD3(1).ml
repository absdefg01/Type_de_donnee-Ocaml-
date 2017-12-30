(***********TP3 Inf�rence de type**********)

(**********1 Type inductifs, types polymorphes**********)
(*Exemple*)
type 'a option = 
    None
  |Some of 'a;;  (*Some permet de v�rifier si la cl� appara�t dans la liste*)

let selection lst = 
  match lst with
    |[]->0
    |(cle',valeur)::r->cle';;
(*Cette fonction qui permet de retourner la cl� de premier �l�m�nt de la liste*)

(*Exemples*)
selection[];;
1::(2::(3::[]));; (*ajouter dans l'ordre 3,2(mettre � gauche de 3),1(mettre � gauche de 2)*)
(1,11)::(2,12)::(3,13)::[];; (*Cr�ation d'une liste avec deux dimension*)

(*Pour tester*)
(*Explication : 
1. Dans cette fonction, il y a un seul param�tre : lst, donc si on veut utiliser cette fonction, on doit entrer une liste.
2. Pour conna�tre la forme de la liste, on peut voir cette ligne (cle,valeur)::r->cle', donc on doit entrer une liste sous forme (cle,valeur)::r, par exemple, (1:,11)::(2,12)::[] permettant d'ajouter des �l�m�nts dans la liste vide [] *)
let test = (1,11)::(2,12)::(3,13)::[];;
selection test;;  (*Donc retourner la cl� de premier �l�ment de la liste*)

let selection lst = 
  match lst with
    |[]->[(0,0)]
    |(cle',valeur)::r->r
;;
(*Cette fonction permet de retourner les �l�ments:r sauf le premier*)

(*Pour tester*)
let test = (1,11)::(2,12)::(3,13)::[];;
selection test;;

(*Exemple pour montrer la fonction de "Some"*)
(*lookup_assoc 3 [] = None car il y a pas d'�l�ment correspondant � la cl� 3
lookup_assoc 1[(1,11)::(2,12)::(3,13)] = Some 11 car l'�l�ment (1,11) correspondant � la cl� 1, donc la valeur est 11
lookup_assoc 3[(1,11)::(2,12)::(3,13)] = Some 13 car l'�l�ment (3,13) correspondant � la cl� 3, donc la valeur est 13
lookup_assoc 5[(1,11)::(2,12)::(3,13)] = None car il n'y a pas d'�l�ment correspondant � la cl� 5
lookup_assoc 3[(3,5)::(1,11)::(2,12)::(3,13)] = Some 5 car le premier �l�ment correspondant � la cl� 3 est (3,5), donc la valeur est 5*)

(*1. Ecrivez une fonction lookup_assoc qui prend une cl� et une liste d'association et renvoie Some (valeur associ�) la cl� appar�tre dans la liste, et None sinon*)
let rec lookup_assoc cle lst= 
  match lst with
    |[]->None
    |(cle',valeur)::r-> if (cle'=cle) then Some valeur else lookup_assoc cle r
;;
(*Explication : 
Cette fonction permet de retourner la valeur qui correspond � la cl�.
1. Si la liste est vide, cela va retourner None
2. Si la liste n'est pas vide, si on arrive � chercher la cl� dans la liste alors on retourne la valeur correspondante sinon on relance la fonction pour la partie reste de la liste
*)

(*Pour tester : Cette fonction contient deux param�tres : cle et lst, donc on doit entrer la cl� qu'on veut chercher et la liste � chercher*)
let test = (1,11)::(2,12)::(3,13)::[];;
lookup_assoc 1 test;; (*Donc retourner la valeur correspondante � la cl� 1, c�d 11*)



(*2. Ecrivez une fonction add_assoc qui prend une cl�, une valeur et une liste d'association et ajoute la paire (cle,valeur) � la liste*)
let add_assoc cle valeur lst = (cle,valeur)::lst;;
(*Explication : Cette fonction permet d'ajouter la paire � une liste*)

(*Pour tester : cette fonction contient 3 param�tres : cle, valeur et liste, donc on duit entrer la cle, la valeur et la liste � ajouter*)
let test = (1,11)::(2,12)::(3,13)::[];
add_assoc 1 2 test;;
lookup_assoc 3 (add_assoc 3 5 test);;
lookup_assoc 2 test;;

(*celui-l� ne marche pas !!!!*)
(*Exo : fonction � ajouter la cl� � la valeur*)
(*let add_assoc cle lst = 
  match lst with
    |[]->None
    |(cle',valeur)::r->if cle'=cle then cle'+ Some valeur else add_assoc cle r;;*)

(*Explication : cette fonction permet d'ajouter la cl� et la valeur dans la liste et de calculer la somme de la cl� et la valeur*)

(*Pour tester : cette fonction contient trois param�tres : cle, valeur et la liste, donc on doit entrer la cl�, la valeur et la liste*)
let test = (1,11)::(2,12)::(3,13)::[];;
add_assoc 1 2 test;;

(*3. Ecrivez une fonction remove_assoc qui prend une cl� et une liste d'association et supprimer la(les) valeur(s) associ�e(s)*)
let rec remove_assoc cle lst = 
  match lst with
    |[]->[]
    |(cle',valeur)::r-> if cle'=cle then r else (cle',valeur)::remove_assoc cle r;;
(*Explication :
Cette fonction permet de supprimer le premier �l�ment qui correspond � la cl� dans la liste*)

(*Pour tester : cette fonction contient deux param�tres : cle et list, donc on doit entrer la cl� de l'�l�ment � supprimer et la liste*)      
let test = (1,11)::(2,12)::(3,13)::(3,15)::[];;
remove_assoc 3 test;;

let rec remove_assoc cle lst = 
  match lst with
    |[]->[]
    |(cle',valeur)::r->if (cle'=cle) then remove_assoc cle' r else (cle',valeur)::remove_assoc cle r;;
(*Explication : cette fonction permet de supprimer tous les �l�ments*)

(*Pour tester : cette fonction contient deux param�tre : cle et list, donc on doit entrer la cl� de l'�l�ment � supprimer et la liste*)
let test = (1,11)::(2,12)::(3,13)::(3,15)::[];;
remove_assoc 3 test;; (*Donc retourner la liste qui ne contient pas d'�l�ments correspondants � la cl�*)

(*faire ces fonction avec une liste tri*)
(*pas encore fini*)
