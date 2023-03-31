val k : float
val d : float
val energie_arete : Maths.point -> Maths.point -> float
val ajout_gradient :
  Maths.projection -> Maths.vecteur array -> float -> Maths.projection
val somme_forces : Maths.vecteur array -> Maths.vecteur
val potentiel_coulomb : 'a -> float
val gradient_energie_arete : Maths.point -> Maths.point -> Maths.vecteur
val resultante_forces :
  int -> Graph.graphe -> Maths.projection -> Maths.vecteur
val gradient_energie :
  Graph.graphe -> Maths.projection -> Maths.vecteur array
val energie : Graph.graphe -> Maths.projection -> float
