(*TP4 Unification*)
(*majuscul -- variables minuscul -- constantes*)
type const = 
	BConst of bool
	|IConst of int
	|FConst of string
;;

type term = 
	Const of const
	|Var of string
	|Appl of term * term
;;
(*--------------------------------------------------------Question 1------------------------------------------------------------*)
(*1 Unification*)
(*Ex : Représentation of term (p(m 3 X) 2) = (p(m 3) X)) 2): *)

(*1. p(m 3 X) 2*)
let tex1 = Appl(Appl(Const (FConst "p"),
			(Appl (
					Appl(Const (FConst "m"), Const (IConst 3)),
					Var "x"))),
			Const (IConst 2))
;;

(*2. p(3 m X) Y*)
let tex2 = Appl(
			Appl(Const (FConst "p"),
				(Appl(
						Appl(Const(IConst 3), Const(FConst "m")),
						Var "x"))),
				Var "y")
;;

(*3. p X (m 3 X)*)
let tex3 = Appl(
				Appl(Const(Var "p"), Const (Var "x")),
				Appl(
					Appl(
						Const(FConst "m"), Const(IConst 3)
						),
					Var "x"
				)
				)
;;



(*--------------------------------------------------------Question 3------------------------------------------------------------*)
(*Q2 Sur le cahier à completer !!*)



(*--------------------------------------------------------Question 3------------------------------------------------------------*)
(*Q3
Indication- 
Ecrire une fonction auxiliaire qui fait l'ensemble de 2 listes 
 List.nom a list(pas de répétitions !!)*)

 (*fonction auxiliaire : union de deux ensembles*)
 let rec union = fun s1 -> fun s2 ->
	match s1 with
	[] -> s2	(*si s1 est vide*)
	|e::s1' -> if List.mem e s2	(*vérifier que e est dans s2 ou pas*)
				then (union s1' s2)	(*Oui, union s1 s2*)
				else e::(union s1' s2)	(*Non, ajouter e dans la liste*)
;;

(*l'enseble des variables libres*)
(*f(2) = {} f(x) = {x} f(s1 s2) = {s1, s2} sans répétition en utilisant la fonction union*)
let rec fv = function
	Const a -> [] (*if c'est un constante, alors retourne une liste vide*)
	|Var b -> [b]	(*if c'est un variable, alors ajouter dans la liste*)
	|Appl(c,d)->union(fv c)(fv d) (*if y a 2 param dans la fonction, appel récursif pour faire les même démarches précédentes*)
;;
fv tex1;;

(*e signfie le premier élément dans la liste
  s1' est la suite de la liste*)
union[1;2;3][2;3;4;5];;	



(*--------------------------------------------------------Question 4------------------------------------------------------------*)
let rec lookup_assoc = fun k -> fun kvs ->
	match kvs with
		[]->None
		|(k',v')::kvs' -> if k=k' then Some v' else lookup_assoc k kvs'
;;
let rec subst_assoc = fin trm -> fun sbst ->
	match trm with
		|const c -> Const c
		|Var v ->
				(match lookup_assoc v sbst with
					None -> Var v
					|Some trm' -> trm')
		|Appl(t1,t2)->Appl(subst_assoc t1 sbst, subst_assoc t2 sbst)
;;

(*Exemple ; Substition m(y x)[x <-(g 4)]*)
subst_assoc
	(Appl(Appl(Const(FConst "m"), Var "y"), Var "x"))
	[("x"),(Appl(Const(FConst "u"), Const(IConst 4))))]
;;

(*--------------------------------------------Question 5 et 6 Substitution dans E et S------------------------------------------*)
Question 5
E est représenter par une liste de couples de termes  E=(t1=^?t2)U....
S est représenté par une liste et association
Rappel
Problème d'unification 
Tester une unification tanque pour (t1) = sigma(t2)

(*Substitution dans E*)
let rec subst_in_term_pair_list es sbst = 
	match es with
		|[]->[]
		|(t1,t2)::es'->
		(subst_assoc t1 sbst, sbst_assoc t2 sbst) :: sinst_in_terme_paire_list es' sbst
;;

(*Substitution dans S
  Precondition : the viariables substituted in S and sbst have to bi disjoint*)
  let rec subst_in_subst s sbst = 
	match s with
		|[]->[]
		|(x, t)::s' ->
		(x, subst_assoc t sbst) :: subst_in_subst s' sbst
;;		
(*--------------------------------------------Question 7 Unification------------------------------------------*)
let rec unif = function
	(*Cas de base*)
	([], s) -> s
	
	|((Const c1, Const c2) ::es, s)->
		if c1 = c2
		then unif(es,s)		(*rule Delet (Variable case)*)
		else failwith "Clash"		(*rule clash*)
	|((Var x1, Var x2)::es,s)->
		if x1 = x2
		then unif(es,s)		(*rule Delete (Variable case) *)
		else
		let sbst = [(x1, Var x2)] in
			unif (subst_in_terme_paire_list es sbst (subst_in_subst s sbst) @ sbst)
		
		(*rule Decomposee*)
		|((Appl(s1, s2), Appl(t1, t2)):: es, s)->unif((s1,t1)::(s2:t2)::es),s)
		
		|((Var x, t) :: es, s)->
			if List.nem x (fv t)
			then failwith "free viariable"	(*rule check*)
			else 		(*rule Eliminaute (other than Variable) *)
			let sbst = [(x,t)] in
				unif (subst_in_term_paire_list es sbst,(subst_in_subst s sbst) @ sbst)
				
		(* rule Eliminate (symmetric case)*)
		| ((r, Var x):: es, sbst)-> 
		
		(* all other cases*)
		|((_,_) :: e, sbst) -> failwith "fail ne pas être unifiable"
;;