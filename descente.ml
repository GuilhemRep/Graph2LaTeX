Random.self_init ()

let () =
    let n = 15 in
    let g = Graph.init_graphe n in
    (*Fonction aléatoire*)
    let gen = Maths.pseudo_aleatoire n in
    let str = ref "" in
    for i=0 to (n/2) do
        Graph.add_edge g (Int.of_float(gen())) (Int.of_float(gen())) 1.;
    done;
    for i=0 to 300 do
        Graph2LaTeX.une_descente g 0.2;
        str := !str ^ (Graph2LaTeX.graph_to_string g);
        str := !str ^ "\\newpage";
    done;

    Graph2LaTeX.write_string_file "graphe.tex" (!str)