Random.self_init ()

let () =
    let n = 40 in 
    let prenoms = Array.init n (fun i-> "$"^(Int.to_string (i+2))^"$") in
    let g = Graph.init_graphe_label prenoms in
    (*Fonction aléatoire*)
    for m=0 to (n-1) do
        for i=0 to m do
            if (m+2) mod (i+2) = 0 then Graph.add_edge_coord g i m 2.;
        done
    done;



    Graph2LaTeX.meilleure_descente g 15000 1;
    print_float (Physics.energie g);
    print_newline();

    Graph2LaTeX.write_string_file "graphe.tex" (Graph2LaTeX.graph_to_string g false)
