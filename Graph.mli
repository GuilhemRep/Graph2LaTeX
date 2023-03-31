type arc = { i : int; w : float; }
type graphe = arc list array
val init_graphe : int -> graphe
val connected : graphe -> int -> int -> bool
val actualise : arc -> arc list -> arc list
val add_edge : graphe -> int -> arc -> unit
