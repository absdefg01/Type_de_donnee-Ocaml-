1.Concevez en Caml des types de données expr(pour les expressions) et stmt(pour des 
instructions/statement) qui permettent de représenter le langage impératif donné par la 
grammaire p.24 des transparents du cours.
type expr = 
	Constint of int
	|ConstBool of bool
	|Var of string
	|Add of expr * expr
	|Equal of expr * expr
	|And of expr * expr
	|Or of expr * expr
	|Inf of expr * expr
	|Sup of expr * expr
	|Moins of expr * expr
	|Multi of expr * expr
;;

type stmt = 
	Aff of string * expr
	|Seq of stmt * stmt
	|If of expr * stmt * stmt
	|While of expr * stmt
;;

2.Représenter les exemples vus en cours instances de ces types de données.
1) (x < 3) && (y > 7)
And(Inf(Var "x", Constint 3), Sup(Var "y", Constint 7));;

2) b == (y + z == 2)
Equal(Var "b",Equal(Add(Var "y", Var "z"), 7));;

3)
while (x < 5) do { 
	x = x + 1;
	y=x-y;
}
ex : 
x = 3
Aff("x", Constint 3);;

While (Inf(Var "x", Constint 5),
	Seq(Aff("x", Add(Var "x", Constint 1)),
	Aff("y", Moins(Var "x", Var "y")))
)
;;

4) 
while (x < 5) || b do { 
x=2*x-1
};
y = x * x;
Seq(While(Or(Inf(Var "x", ConstInt 5), Var "b"),
	Aff("x",Moins(Multi(Constint 2, Var "x"), ConstInt 1)),
	Aff("y", Multi(Var "x", Var "x"))
);;

3.Ecrivez les fonctions qui permettent d'imprimer des expressions/instructions.
let rec print_expr e= 
	(match e with
	|Constint i -> string_of_int i
	|ConstBool true -> "true"
	|ConstBool false -> "false"
	|Var x->x
	|Add(e1,e2) -> "(" ^ (print_expr e1) ^ "+" ^ (print_expr e2) ^ ")"
	|Moins(e1,e2) -> "(" ^ (print_expr e1) ^ "-" ^ (print_expr e2) ^ ")"
	|Equal(e1,e2) -> "(" ^ (print_expr e1) ^ "=" ^ (print_expr e2) ^ ")"
	|And(e1,e2) -> "(" ^ (print_expr e1) ^ "&&" ^ (print_expr e2) ^ ")"
	|Or(e1,e2) -> "(" ^ (print_expr e1) ^ "||" ^ (print_expr e2) ^ ")"
	|Inf(e1,e2) -> "(" ^ (print_expr e1) ^ "<" ^ (print_expr e2) ^ ")"
	|Sup(e1,e2) -> "(" ^ (print_expr e1) ^ ">" ^ (print_expr e2) ^ ")"
	|Multi(e1,e2) -> "(" ^ (print_expr e1) ^ "*" ^ (print_expr e2) ^ ")"
);;

ex : print_expr(Add(Var "y", Constint 2));;

imprimer : (x < 3) && (y > 7)
print_expr(And(Inf(Var "x", Constint 3), Sup(Var "y", Constint 7)));;
	
imprimer : b == (y + z == 2)
print_expr(Equal(Var "b", Equal(Add(Var "x", Var "y"), Constint 2)));;

type stmt = 
	Aff of string * expr
	|Seq of stmt * stmt
	|If of expr * stmt * stmt
	|While of expr * stmt
;;

这个不会写
let rec print_expr stmt = 
	(match stmt with
	Aff(s,e)->"(" ^ "s" ^ "=" ^ (print_expr e) ^ ")" 
	|Seq(stmt1,stmt2) -> "(" ^ (print_expr stmt1) ^ (print expr stmt2) ^ ")"
	|If(e,stmt1,stmt2) -> "(" ^ "if" ^ (print_expr e) ^ "then" ^ (print_expr stmt1) ^ "else" ^ (print_expr stmt2) ^ ")"
	|While(e,stmt1) ->"(" ^ "While" ^ (print_expr e) ^ "do" ^ (print_expr stmt1) ^ "done" ^ ")"
);;

4.Concevez un type de données tp pour représenter les types élémentaires.
type tp = Int|Bool|Void;;

5.Concevez un structure de données tp pour représenter des environnements. Implantez les
algorithmes "essentiels" pour la manipulation de cette structure de données (ajout d'une
déclaration, recherche d'un type associé au nom d'une variable).

这个不会写
let rec verif_tp_expr e = 
	match e with
	|Constint i -> "int"
	|ConstBool b -> "bool"
	|Add(e1,e2)->
		let tp1 verif_tp_expr e1 in
		let tp2 verif_tp_expr e2 in
		if tp1 = int && tp2 = int then print_string "int" else failwith "erreur de type";;
		
	
	
	
	