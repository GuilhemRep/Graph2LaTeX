val k : float
val d : float
val energie_arete : Maths.point -> Maths.point -> float
val ajout_gradient : Graph.graphe -> Maths.vecteur array -> float -> unit
val potentiel_coulomb : float -> float
val gradient_energie_arete : Maths.point -> Maths.point -> Maths.vecteur
val resultante_forces : int -> Graph.graphe -> Maths.vecteur
val gradient_energie : Graph.graphe -> Maths.vecteur array
val energie : Graph.graphe -> float
