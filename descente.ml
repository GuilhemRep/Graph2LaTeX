Random.self_init ()

let () =
    let n = 10 in
    let g = Graph.init_graphe n in
    (*Fonction aléatoire*)
    let gen = Maths.pseudo_aleatoire n in
    for i=0 to (n/2) do
        Graph.add_edge g (Int.of_float(gen())) (Int.of_float(gen())) 1.;
    done;
    Graph2LaTeX.meilleure_descente g 1000 100;
    Graph2LaTeX.write_file "graphe.tex" g