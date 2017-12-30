let rec append xs ys = 
	match xs with
	|[] -> ys
	|x::xs' -> x::append xs' ys;
(*append [] ys = ys
append (x::xs') ys = xs::append xs' ys*)

preuve de : pour tout xs, ys
length(append xs ys) = length xs + length ys

cas de base : 
à montrer : 
length(append [] ys) = length[] + length ys	
1 - length(append [] ys) = length ys (*car append [] ys = ys*)
2 - length[] + length ys = length ys (*car length[] = 0*)
donc 1 == 2

cas inductif :
hypothèse pour un xs' fixe :
length(append xs' ys) = length xs' + length ys

à montrer : 
length(append (x::xs') ys) = length (x::xs') + length ys

1 - length(append (x::xs') ys) 
= length (xs::append xs' ys)(*car append (x::xs') ys = xs::append xs' ys*)
= 1 + length (append xs' ys) (*car length append xs' ys = 1 + length (append xs' ys)*)
= 1 + length xs' + length ys (*car hypothèse*)

2 - length(x::xs') + length ys
= 1 + length xs' + length ys (*car length(xs::xs') = 1 + length xs'*)

donc 1 == 2





let p = fun x -> x mod 2 = 0;;
let rec filtrer p xs = 
	match xs with
	|[] -> []
	|x::xs'->if(p x) then x::filtrer p xs' else filtrer p xs'
;;
(*
filtrer p [] = []
filtrer p (x::xs') = if (p x) then x::filtrer p xs else filtrer p xs
*)

Preuve de : pour tout p, xs
filtrer p (filtrer p xs) = filtrer p xs

cas de bas :
à montrer : filtrer p (filtrer p []) = filtrer p []
1 - filtrer p (filtrer p []) 
= filtrer p [] (*car filtrer p [] = []*)
= [] (*car filtrer p [] = []*)
2 - filtrer p []
=[] (*car filtrer p [] = []*)
donc 1 == 2

Cas inductif :
Hypothèse : pour un xs' fixe :
filtrer p (filtrer p xs') = filtrer p xs'
à montrer :
filtrer p (filtrer p (x::xs')) = filtrer p (x::xs')
1 - filtrer p  (filtrer p (x::xs'))
= filtrer p (if (p x) then x::filtrer p xs' else filtrer p xs')
(*car filtrer p (x::xs') = if (p x) then x::filtrer p xs' else filtrer p xs'*)

(*car on a f (if c then t) = if c then f t else f e*)
donc on a 
1 - if (p x) then x :: (filtrer p xs') else filtrer p xs'
= if (p x) then
	(if (p x) then x::filtrer p (filtrer p xs'))
	else filtrer p filtrer p (filtrer p xs')
else filtrer p xs'


