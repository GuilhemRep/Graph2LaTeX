type arc = {i:int ; w:float} (* but,poids *)

(* Tableau d'arcs : la case i contient la liste des arcs {j ; w} tels que i->j est un arc de poids w *)
type graphe = {g : (arc list) array ; mutable p : Maths.projection}

(* Renvoie une projection pseudo-aléatoire de [n] points distints*)
let init_projection gen (n:int) =
  (* Ajoute [n] points à [l] sans doublons *)
  let rec init n l =
    if n = 0 then l
    else (
      let p:Maths.point = {x=gen() ; y=gen()} in
      if List.mem p l then init n l (* Nouvel essai si point deja present dans la liste *) 
      else init (n-1) (p::l)
    ) in
  
  (Array.of_list (init n []):Maths.projection)


(* Renvoie une nouvelle projection décallée pour avoir des coordonnées latex plus petites *)
let decale (graphe:graphe) =
  let n = Array.length graphe.p in
  let p = graphe.p in
  let minx,miny = ref max_float, ref max_float in
  for i=0 to (n-1) do
    minx := min (!minx) (graphe.p.(i).x);
    miny := min (!miny) (graphe.p.(i).y);
  done;
  for i=0 to (n-1) do
    p.(i)<- {x = graphe.p.(i).x -. (!minx) ; y = graphe.p.(i).y -. (!miny)}
  done;
  ({g=graphe.g ; p = p}:graphe)

(* Crée un graphe de taille [n] *)
let init_graphe (n:int) =
  assert (n>=0);
  ({g = Array.make n [] ; p = init_projection (Maths.pseudo_aleatoire n) n}:graphe)


(* Change la projection du graphe et la remplace par une autre random *)
let shake (graphe:graphe) gen =
  graphe.p <- init_projection gen (Array.length graphe.g)

(* True si [s1] est voisin de [s2] dans [graphe], false sinon *)
let connected (graphe:graphe) (s1:int) (s2:int) =
  let rec aux l = match l with
    [] -> false
    |a::q -> (a.i == s2) || aux q
  in 
    aux (graphe.g.(s1))

(* Ajoute l'arc d'arrivée [y] et de poids [w] à la liste d'arcs [l]. Si un arc d'arrivée [y] est
déjà présent, il est écrasé par le nouvel arc*)
let rec actualise (a:arc) (l:arc list) = match l with
  [] -> [a]
  |t::q when t.i == a.i -> a::q
  |t::q -> t::(actualise a q)

(* Ajoute l'arête [x]->[y] de poids [weight] dans le graphe [graphe] *)
let add_edge (graphe:graphe) (x:int) (y:int) (w:float) =
  (graphe.g).(x) <- actualise {i=y ; w=w} (graphe.g).(x)