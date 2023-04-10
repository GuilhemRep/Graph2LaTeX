Random.self_init ()

let () =
    let n = 12 in
    let g = Graph.init_graphe n in
    (*Fonction aléatoire*)
    let gen = Maths.pseudo_aleatoire n in
    for i=0 to 12 do
        (*let a = Int.of_float(gen()) and b = Int.of_float(gen()) in*)
        let a = Random.int n and b = Random.int n in
        if a<>b then (
        Graph.add_edge g a b (Float.of_int (1+Random.int 10));
        )
    done;

    Graph2LaTeX.meilleure_descente g 6000 3;

    Graph2LaTeX.write_string_file "graphe.tex" (Graph2LaTeX.graph_to_string g)