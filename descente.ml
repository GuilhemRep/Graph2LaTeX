Random.self_init ()

let () =
    let n = 15 in
    let g = Graph.init_graphe n in
    for i=0 to (n-1) do
        Graph.renomme_etiquette g i ("$P_{" ^ (Int.to_string i) ^ "}$")
    done;
    (*Fonction aléatoire*)
    let gen = Maths.pseudo_aleatoire n in
    let str = ref "\\newpage \\newpage" in
    for i=0 to 20 do
        let a = Int.of_float(gen()) and b = Int.of_float(gen()) in
        Graph.add_edge_coord g a b (Float.of_int (1+Random.int 10));
    done;
    for i=0 to 40000 do
        Graph2LaTeX.une_descente g 0.2;
        if i mod 300 == 0 then (
        str := !str ^ (Graph2LaTeX.graph_to_string g false);
        str := !str ^ "\\newpage")
    done;

    Graph2LaTeX.write_string_file "graphe.tex" (!str)