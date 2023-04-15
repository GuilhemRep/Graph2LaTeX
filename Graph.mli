type arc = { i : int; w : float; }
type graphe = {
  g : arc list array;
  mutable l : string array;
  mutable p : Maths.projection;
}
val init_projection : 'a -> int -> Maths.projection
val decale : graphe -> graphe
val init_graphe : int -> graphe
val init_graphe_label : string array -> graphe
val renomme_etiquette : graphe -> int -> string -> unit
val shake : graphe -> 'a -> unit
val connected : graphe -> int -> int -> bool
val actualise : arc -> arc list -> arc list
val add_edge_coord : graphe -> int -> int -> float -> unit
val add_edge : graphe -> string -> string -> float -> unit
val croisements : graphe -> int
