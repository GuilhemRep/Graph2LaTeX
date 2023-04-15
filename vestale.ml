Random.self_init ()

let () =
    let prenoms = [|"Antoinette"; "Sylvie"; "Marie"; "Andrée"; "Simone"; "Josette"; "Jeanne"; "Léonie"; "Sidonie"; "Nina"|] in
    let n = Array.length prenoms in
    let g = Graph.init_graphe_label prenoms in

    (*Fonction aléatoire*)
    let gen = Random.int n in
    for i=0 to 4*n/3 do
        (*let a = Int.of_float(gen()) and b = Int.of_float(gen()) in*)
        let a = Random.int n and b = Random.int n in
        if a<>b then (
        Graph.add_edge_coord g a b (Float.of_int (1+Random.int 10));
        )
    done;

    Graph2LaTeX.meilleure_descente g 12000 3;
    print_float (Physics.energie g);
    print_newline();

    Graph2LaTeX.write_string_file "graphe.tex" (Graph2LaTeX.graph_to_string g false)