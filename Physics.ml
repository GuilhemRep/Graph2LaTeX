open Graph

(* Définition de [k] (constante de Hooke) et [d] (élongation à vide d'une arête) *)
let k = 0.5 and d = 5.

(* Loi de Hooke : renvoie l'énergie potentielle entre les points [p1] et [p2]*)
let energie_arete (p1:Maths.point) (p2:Maths.point) =
  -. k *. (Maths.pow (Maths.distance p1 p2 -. d) 2)

(* Prend une projection [proj], un tableau de vecteurs [v], un réel [delta] et modifie la projection :
  chaque [proj.(i)] devient [proj.(i) + delta*v.(i)]*)
let ajout_gradient (graphe:Graph.graphe) (v:Maths.vecteur array) (delta:float) =
  graphe.p<- Maths.somme_tableaux graphe.p (Maths.mult_scal_tableau v delta)

(*let potentiel_coulomb x =(1./.1000.)*)

let potentiel_coulomb x =
  assert (x>0.);
  (*let c = (1. /. x -. 1. /. (x*.x))/.10. and m = 0.000001 in
  let c = -.(1. /. x)/.10. and m = 0.000001 in
  if c<m then c else c*)
  max 0. ((x-.d)/.1000.)


(*
Calcule le gradient de l'énergie d'une arête selon la formule :
E(X) = -k (l(X) - d)² = -k (sqrt(x²+y²) - d)²
gradE(X) = -(2k (sqrt(x²+y²) - d)/sqrt(x²+y²)) X = -2k (l(x)-d)/(l(x)) X
*)
let gradient_energie_arete (p1:Maths.point) (p2:Maths.point) =
  let l = Maths.distance p1 p2 in
  assert (not (Float.is_nan l));
  assert (l<>0.);
  let t = -.2.*.k*. (l-.d)/.l in
  ({x = t*. (p1.x -. p2.x) ; y = t*. (p1.y -. p2.y)}:Maths.vecteur)


(* Force appliquée à un sommet [s]. *)
let resultante_forces (s:int) (graphe:Graph.graphe) =
  let coord_s = graphe.p.(s) in

  let rec aux accumulateur liste_voisins = match liste_voisins with
    []-> accumulateur
    |voisin::q when voisin.i == s -> aux accumulateur q
    |voisin::q -> 
      (
        let coord_voisin = graphe.p.(voisin.i) in
        let attraction = -.potentiel_coulomb ((Maths.distance coord_s coord_voisin)/.d) in 
        (*print_float (Maths.distance coord_s coord_voisin); print_string "  "; print_float attraction; print_newline ();*)
        aux (Maths.somme_vecteurs accumulateur (Maths.mult_scal_vecteur (gradient_energie_arete coord_s coord_voisin) attraction)) q
      )
    in

  (* Attraction : les sommets reliés à p *)
  let n = Array.length graphe.g in
  let force = ref (aux (Maths.init_vecteur()) (graphe.g.(s):Graph.arc list) ) in
  (* Répulsion : les sommets non-reliés *)
  for i=0 to (n-1) do
    if (i <> s) && not (Graph.connected graphe s i) then
      let coord_point = graphe.p.(i) in
      let repulsion = ( potentiel_coulomb (Maths.distance coord_s coord_point) ) in 
      print_float repulsion; print_newline ();
      force := Maths.somme_vecteurs (!force) (Maths.mult_scal_vecteur (gradient_energie_arete coord_s (graphe.p.(i))) 0.03);
    done;
  assert (not (Float.is_nan (!force.x)));
  assert (not (Float.is_nan (!force.y)));
  (!force)


(* Renvoie un tableau de vecteurs de taille n correspondant au gradient
de l'énergie du graphe représenté par sa projection *)
let gradient_energie (graphe:Graph.graphe) =
  let taille = Array.length graphe.g in
  let g = Array.make taille (Maths.init_vecteur ()) in
    for p=0 to (taille -1) do
      g.(p) <- resultante_forces p graphe
    done;
    g

(* Calcule l'énergie potentielle d'un graphe [g] *)
let energie (graphe:Graph.graphe) =
  let rec aux l point = match l with
    []    -> 0.
    |(a:Graph.arc)::q -> energie_arete (graphe.p.(a.i)) point +. aux q point in
  let e = ref 0. in
  for i=0 to (Array.length graphe.g - 1) do
    e := !e +. aux (graphe.g.(i)) (graphe.p.(i))
  done;
  !e