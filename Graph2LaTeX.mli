val print_graphe : Graph.graphe -> unit
val une_descente :
  Graph.graphe -> Maths.projection -> float -> Maths.projection
val descente : Graph.graphe -> (unit -> float) -> int -> Maths.projection
val meilleure_descente : Graph.graphe -> int -> int -> Maths.projection
val write_file : string -> Graph.graphe -> Maths.projection -> unit
