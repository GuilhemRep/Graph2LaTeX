open Graph

(* Définition de [k] (constante de Hooke) et [d] (élongation à vide d'une arête) *)
let k = 0.5 and d = 3.

(* Loi de Hooke : renvoie l'énergie potentielle entre les points [p1] et [p2]*)
let energie_arete (p1:Maths.point) (p2:Maths.point) =
  -. k *. (Maths.pow (Maths.distance p1 p2 -. d) 2)

(* Prend une projection [proj], un tableau de vecteurs [v], un réel [t] et renvoie
  une nouvelle projection actualisée : chaque [proj.(i)] devient [proj.(i) + t*v.(i)]*)
let ajout_gradient (proj:Maths.projection) (v:Maths.vecteur array) (delta:float) =
  (Maths.somme_tableaux proj (Maths.mult_scal_tableau v delta) : Maths.projection)


(* Renvoie le vecteur v1 + ... + vn du tableau [tab = [|..., vi,...|] ] *)
let somme_forces (tab : Maths.vecteur array) =
  let tot = ref (Maths.init_vecteur()) in
  for i=0 to (Array.length tab - 1) do
    tot := Maths.somme_vecteurs (!tot) (tab.(i))
  done;
  (!tot:Maths.vecteur)



let potentiel_coulomb x =(1./.10000000.)

(*let potentiel_coulomb x = min (10. /. x -. 10. /. (Maths.pow x 2)) (1./.100000000.)*)  


(*
Calcule le gradient de l'énergie d'une arête selon la formule :
E(X) = -k (l(X) - d)² = -k (sqrt(x²+y²) - d)²
gradE(X) = -(2k (sqrt(x²+y²) - d)/sqrt(x²+y²)) X = -2k (l(x)-d)/(l(x)) X
*)
let gradient_energie_arete (p1:Maths.point) (p2:Maths.point) =
  let l = Maths.distance p1 p2 in
  assert (not (Float.is_nan l));
  let t = -.2.*.k*. (l-.d)/.l in
  ({x = t*. (p1.x -. p2.x) ; y = t*. (p1.y -. p2.y)}:Maths.vecteur)


(* Force appliquée à un sommet [s]. *)
let resultante_forces (s:int) (graphe:Graph.graphe) (proj:Maths.projection) =
  let coord_s = proj.(s) in

  let rec aux accumulateur liste_voisins = match liste_voisins with
    []-> accumulateur
    |voisin::q -> 
      (
        let coord_voisin = proj.(voisin.i) in
        let attraction = (potentiel_coulomb ((Maths.distance coord_s coord_voisin)/.d)) in 
        aux (Maths.somme_vecteurs accumulateur (Maths.mult_scal_vecteur (gradient_energie_arete coord_s coord_voisin) attraction)) q
      )
    in

  (* Attraction : les sommets reliés à p *)
  let n = Array.length graphe in
  let force = ref (aux (Maths.init_vecteur()) (graphe.(s):Graph.arc list) ) in
  (* Répulsion : les sommets non-reliés *)
  for i=0 to (n-1) do
    if (i <> s) && not (Graph.connected graphe s i) then
      let coord_point = proj.(i) in
      let repulsion = potentiel_coulomb (Maths.distance coord_s coord_point) in 
      force := Maths.somme_vecteurs (!force) (Maths.mult_scal_vecteur (gradient_energie_arete coord_s (proj.(i))) repulsion);
  done;
  (!force)


(* Renvoie un tableau de vecteurs de taille n correspondant au gradient
de l'énergie du graphe représenté par la projection [proj] *)
let gradient_energie (graphe:Graph.graphe) (proj:Maths.projection) =
  let taille = Array.length graphe in
  let g = Array.make taille (Maths.init_vecteur ()) in
    for p=0 to (taille -1) do
      g.(p)<- resultante_forces p graphe proj
    done;
    g

(* Calcule l'énergie potentielle d'un graphe [g] *)
let energie (graphe:Graph.graphe) (proj:Maths.projection) =
  let rec aux l point = match l with
    []    -> 0.
    |(a:Graph.arc)::q -> energie_arete (proj.(a.i)) point +. aux q point in
  let e = ref 0. in
  for i=0 to (Array.length graphe - 1) do
    e := !e +. aux (graphe.(i)) (proj.(i))
  done;
  !e