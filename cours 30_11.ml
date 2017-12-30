(*transparants Page 175*)
(*TD 6*)
(*2.1*)
let rec map = fun f xs ->
  match xs with
    |[]->[]
    |x::xs'->f x::map f xs'
;; 

let rec from k = k::(from(k+1));;

let rec fromq = fun k ->
  Cons(k,fun()->fromq(k+1))
;;

let rec takeq n = function
Nil->[]
  |Cons(x,xq)->
    if n=0 then [] else x::(takeq (n-1) (xq()));;


let rec take n l = 
  match l with
    |[]->[]
    |(x::xs)->if(n = 0) then [] else x::(take(n-1) xs)
;;
 

type 'a seq = 
  |Nil
  |Cons of 'a * (unit->'a seq)
;;



let rec mapq = fun f xq->
  match xq with
    |Nil->Nil 
    |Cons (x,xq') -> Cons ((f x), fun()->mapq f (xq'()))
;;


let rec filtrer =  fun p xs ->
  match xs with
    |[]->[]
    |(x::xs')->
      if p x then x::filtrer p xs'
      else filtrer p xs';;       

let rec filtrerq = fun p xs ->
  match xs with
    |Nil->Nil
    |Cons (x,xs')-> if (p x) then Cons (x,fun()->filtrerq p (xs'()))
      else filtrerq p (xs'());;

let select_non_multiples p xq = (filtrerq (fun r->r mod p <>0)xq);;
let rec sieve = function
    |Nil->Nil
    |Cons(p,rest)->Cons(p, fun()->sieve(select_non_multiples p (rest())))
;;

let primes = sieve (fromq 2);;





type('a,'b)graph = ('a*'a*'b)list;;

let exg1 = [("a","b",2);("a","c",4);("b","c",1);("d","b",3);("b","d",3);()]

type ('a,'b)state = ('a*'a list *'b);;

let sol_state : (('a,'b) state ->bool)=function
(g,[],_)->failwith "invalid state ; no start node"
(g,v::vs,cc)->g = v
;;

let nex_nodes n gr =
LIST.FILTER(FUN(S,T,C)->S=n) gr

let progres parcours arcs = List.filter (fun(s,t,c)->not (List.mem t parcours)) arcs
;;

let extend_state cd (g.parcours.cacc)(s,t,cacc) (s,t,carc) = 
  (g, t::parcours, cf cacc carc)

let next_states gr cf = function
(g,[],_)->failwith ""
  |(g,v::vs,cc)->
    if g= v then
      [] else
      let nn = next_nodes v gr in
      let new_nn = postgres (v;;vs)
