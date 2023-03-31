type point = {x:float ; y:float} 
type vecteur = {x:float ; y:float} 

type projection = point array

(* Exponentiation rapide : calcule x^n *)
let rec pow (x:float) (n:int) =
  if n = 0 then 1.
  else if n = 1 then x
  else if n mod 2 = 0 then let c = (pow x (n/2)) in c*.c
  else let c = (pow x (n/2)) in c*.c*.x

(* Raccourci infixe *)
let ( ** ) x n = pow x n

let () = assert (2. ** 10 = 1024.)

(* Renvoie un itérateur [suivant] qui à chaque appel renvoie un flottant différent entre 0 et [n] *)
let pseudo_aleatoire n =
  let i = ref 42 in
  let m = 2147483647 in
  let suivant () =
    i := (16807 * (!i) + 17) mod m;
    Float.of_int ((!i) mod (n))
  in
    suivant


(* Renvoie un objet de type point *)
let init_point ():point = 
  {x=0. ; y=0.}

(* Renvoie un objet de type vecteur *)
let init_vecteur ():vecteur = 
  {x=0. ; y=0.}




(* Distance euclidienne entre les points [p1] et [p2]*)
let distance (p1:point) (p2:point) =
  Float.sqrt (pow (p1.x -.p2.x) 2 +. pow (p1.y-.p2.y) 2)

(* Renvoie un NOUVEAU tableau des vecteurs [tab] multipliés par le scalaire [t] *)
let mult_scal_tableau (tab:vecteur array) (t:float) =
  (Array.init (Array.length tab) (fun i -> let p = tab.(i) in {x = p.x *. t; y = p.y *. t}): vecteur array)

(* Multiplie un vecteur [v] par un réel [t] *)
let mult_scal_vecteur (v:vecteur) (t:float) =
  ({x = v.x *. t ; y = v.y *. t}:vecteur)

let norme_vecteur (v:vecteur) =
  sqrt (v.x ** 2 +. v.y ** 2)

let normalise (v:vecteur) =
  assert (norme_vecteur v <> 0.);
  mult_scal_vecteur v (norme_vecteur v)

(* Ajoute les vecteurs [v1] et [v2] *)
let somme_vecteurs (v1:vecteur) (v2:vecteur) =
  ({x = v1.x +. v2.x ; y = v1.y +. v2.y}:vecteur)

(* Ajoute le point [p] au vecteur [v], et renvoie un nouveau point *)
let somme_point_vecteur (p:point) (v:vecteur) =
  ({x = p.x +. v.x ; y = p.y +. v.y}:point)

(* Renvoie un NOUVEAU tableau de points, somme des points de [t1] avec les vecteurs de [t2] *)
let somme_tableaux (t1:point array) (t2:vecteur array) =
  let n = Array.length t1 in
  assert (n = Array.length t2);
  let t = Array.make n (init_point ()) in
  for i=0 to (n-1) do
    t.(i) <- somme_point_vecteur (t1.(i)) (t2.(i))
  done;
  t

(* Norme infinie d'un tableau [v] de vecteurs = max des normes des vecteurs *)
let norme (v:vecteur array) =
  let n = ref 0. in
  for i=0 to (Array.length v - 1) do
    n:= max (!n) (v.(i).x ** 2 +. v.(i).y ** 2)
  done;
  Float.sqrt (!n)