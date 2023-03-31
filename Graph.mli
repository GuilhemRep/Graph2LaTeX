type arc = { i : int; w : float; }
type graphe = { g : arc list array; p : Maths.projection; }
val init_projection : (unit -> float) -> int -> Maths.projection
val decale : graphe -> graphe
val init_graphe : int -> graphe
val connected : graphe -> int -> int -> bool
val actualise : arc -> arc list -> arc list
val add_edge : graphe -> int -> arc -> unit
