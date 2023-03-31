type arc = {i:int ; w:float} (* but,poids *)

(* Tableau d'arcs : la case i contient la liste des arcs {j ; w} tels que i->j est un arc de poids w *)
type graphe = (arc list) array

(* Crée un graphe de taille [n] *)
let init_graphe (n:int) =
  assert (n>=0);
  (Array.make n []:graphe)


(* Répond si x est voisin de y dans le graphe *)
let connected (graphe:graphe) (s1:int) (s2:int) =
  let rec aux l = match l with
    [] -> false
    |a::q -> (a.i == s2) || aux q
  in 
    aux (graphe.(s1))

(* Ajoute l'arc d'arrivée [y] et de poids [w] à la liste d'arcs [l]. Si un arc d'arrivée [y] est
déjà présent, il est écrasé par le nouvel arc*)
let rec actualise (a:arc) (l:arc list) = match l with
  [] -> [a]
  |t::q when t.i == a.i -> a::q
  |t::q -> t::(actualise a q)

(* Ajoute l'arête [x]->[y] de poids [weight] dans le graphe [graphe] *)
let add_edge (graphe:graphe) (x:int) (a:arc) =
  (graphe).(x) <- actualise a (graphe).(x)