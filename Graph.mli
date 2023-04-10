type arc = { i : int; w : float; }
type graphe = {
  g : arc list array;
  mutable l : string array;
  mutable p : Maths.projection;
}
val init_projection : (unit -> float) -> int -> Maths.projection
val decale : graphe -> graphe
val init_graphe : int -> graphe
val renomme_etiquette : graphe -> int -> string -> unit
val shake : graphe -> (unit -> float) -> unit
val connected : graphe -> int -> int -> bool
val actualise : arc -> arc list -> arc list
val add_edge : graphe -> int -> int -> float -> unit
val croisements : graphe -> int
