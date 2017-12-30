(*****************************************TD 6*********************************************)

(*******************************I induction sur les listes*********************************)
let list = [1;2;3];;
let list1 = [3;4;5];;
(*la somme de tous les �l�ments d'une liste d'entiers*)
let rec sum lst = 
	match lst with
	|[]->0
	|n::lst'->n+sum(lst')
;;

(*calculer la longeur d'une liste*)
let rec length lst = 
	match lst with
	|[]->0
	|n::lst' -> 1 + (length lst')
;;

(*concatener deux listes*)
let rec append lst lst2 = 
	match lst with
	|[]->lst2
	|x::lst'->x::(append lst' lst2)
;;

(*prendre une liste de listes  et concatener tous ses �l�ments*)
let rec concat lst = 
	match lst with
	|[]->[]
	|x::lst'->x::(concat lst')
;;

(*prendre une liste et renvoie la liste des �l�ments congrus � 0 modulo 2*)
let rec modulo lst= 
	match lst with
	|[]->[]
	|x::lst'-> (x mod 2)::(modulo lst');; 

(*prendre un pr�dicat et une liste et rencoie tous les �l�ments de la liste qui v�rifient le pr�dicat*)
(*pour tester*)
let p = fun x-> x mod 2 = 0;;

let rec filtrer p lst = 
	match lst with
	|[]->[]
	|n::lst'->if (p n) then n::(filtrer p lst') else filtrer p lst'
;;	


(*******************************II Multi - ensembles*********************************)

type 'a multiset = 'a ->int

(*repr�senter le multi-ensemble vide*)
let m_empty = fun n ->0;;

(*ajouter un �l�ment � un multiensemble*)
let m_add = 
	fun e mset -> fun x -> if (x=e) then (mset x)+1 else (mset x);;
(*mset est multi-ensemble*)

fun e->0
