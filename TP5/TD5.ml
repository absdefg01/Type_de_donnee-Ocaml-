(*TP5*)
(*---------------------------Question 3--------------------------------*)
(* sum qui prend la somme de tous les éléments d'une liste d"entiers*)
let l1 = [1;2;3];;
let l2 = [4;5;6;7;8];;

let rec sum lst = 
  match lst with
    |[]->0
    |e::lst'->e + sum lst';;
 (*Pour tester*)
sum l1;;
sum l2;;

(*length qui calcule la longueur d'une liste d'entiers*)
let rec length lst = List.length lst;;
(*Pour tester*)
length l1;;
length l2;;
let rec length2 lst = 
  match lst with
    |[]->0
    |e::lst'->1 + length lst';;
length2 l1;;
length2 l2;;

(*append qui concatène deux listes*)
let rec append lst1 lst2 = 
  match lst1 with
    |[]->lst2
    |e::lst'->e::append lst' lst2;;
append l1 l2;;

(*concat qui prend une lsite de listes et concatène tous ses éléments*)
(*concat [[1;2];[];[3;4;5]]*)
(*retourner [1;2;3;4;5]*)
let rec concat lst= 
  match lst with
    |[]->[]
    |e::lst'->e @ concat lst';;
(*Pour tester*)
let l3 = [[1;2];[2;4;5]];;
concat l3;;

(*modulo qui prend une liste et renvoie la liste des éléments congrus à 0 modulo 2*)
let rec modulo lst= 
	match lst with
	|[]->[]
	|x::lst'-> (x mod 2)::(modulo lst');; 
modulo l1;;

(*filtrer qui prend un prédicat et une liste et renvoie tous les éléments de la liste qui vérifient le prédicat*)
let p = fun x -> x mod 2 = 0;;
let rec filtrer p lst = 
	match lst with
	|[]->[]
	|n::lst'->if (p n) then n::(filtrer p lst') else filtrer p lst'
;;
filtrer p l1;;
