Random.self_init ()

let () =
    let n = 20 in
    let g = Graph.init_graphe n in
    (*Fonction aléatoire*)
    let gen = Maths.pseudo_aleatoire n in
    for i=0 to (n/2) do
        Graph.add_edge g (Int.of_float(gen())) {i=(Int.of_float(gen())) ; w=1.};
    done;

    let g' = ref g in
    for i=0 to 200 do
        if (true) then (Graph2LaTeX.write_file "graphe.tex" (!g'))
        else ();
        g' := Graph2LaTeX.une_descente g (2.2)
    done

