Random.self_init ()

let () =
    let prenoms = [|"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";|] in
    let g = Graph.init_graphe_label prenoms in
    (*Fonction aléatoire*)
    Graph.add_edge g "a" "b" 2.;
    Graph.add_edge g "b" "c" 2.;
    Graph.add_edge g "c" "a" 2.;
    Graph.add_edge g "b" "d" 2.;
    Graph.add_edge g "d" "e" 2.;
    Graph.add_edge g "e" "f" 2.;
    Graph.add_edge g "e" "g" 2.;
    Graph.add_edge g "f" "h" 2.;
    Graph.add_edge g "g" "h" 2.;
    Graph.add_edge g "f" "i" 2.;
    Graph.add_edge g "h" "j" 2.;
    Graph.add_edge g "g" "j" 2.;
    Graph.add_edge g "g" "k" 2.;
    Graph.add_edge g "k" "l" 2.;
    Graph.add_edge g "l" "m" 2.;
    Graph.add_edge g "m" "k" 2.;


    Graph2LaTeX.meilleure_descente g 6000 10;
    print_float (Physics.energie g);
    print_newline();

    Graph2LaTeX.write_string_file "graphe.tex" (Graph2LaTeX.graph_to_string g)