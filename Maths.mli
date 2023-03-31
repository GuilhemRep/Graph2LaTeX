type point = { x : float; y : float; }
type vecteur = { x : float; y : float; }
type projection = point array
val pow : float -> int -> float
val ( ** ) : float -> int -> float
val pseudo_aleatoire : int -> unit -> float
val init_point : unit -> point
val init_vecteur : unit -> vecteur
val init_projection : (unit -> float) -> int -> projection
val distance : point -> point -> float
val mult_scal_tableau : vecteur array -> float -> vecteur array
val mult_scal_vecteur : vecteur -> float -> vecteur
val norme_vecteur : vecteur -> float
val normalise : vecteur -> vecteur
val somme_vecteurs : vecteur -> vecteur -> vecteur
val somme_point_vecteur : point -> vecteur -> point
val somme_tableaux : point array -> vecteur array -> point array
val norme : vecteur array -> float
val decale : projection -> projection
