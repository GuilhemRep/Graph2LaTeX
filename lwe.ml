Random.self_init ()

let () =
    let problems = [|
    "Base courte";
    "LWE$_{n,q,\\alpha}$";
    "Chiffrement Regev";
    "ISIS";
    "Echantillon gaussien";
    "SIS$_{q,m,\\beta}$";
    "SIVP$_\\gamma$";
    "GapSVP$_\\gamma$";
    "Hashage Ajtai"|] in
    let n = Array.length problems in
    let g = Graph.init_graphe_label problems in

    Graph.add_edge_coord g 0 7 1.;
    Graph.add_edge_coord g 1 2 1.;
    Graph.add_edge_coord g 0 3 1.;
    Graph.add_edge_coord g 0 6 1.;
    Graph.add_edge_coord g 0 4 1.;
    Graph.add_edge_coord g 6 5 1.;
    Graph.add_edge_coord g 7 1 1.;
    Graph.add_edge_coord g 5 8 1.;


    Graph2LaTeX.meilleure_descente g 12000 3;
    print_float (Physics.energie g);
    print_newline();

    Graph2LaTeX.write_string_file "graphe.tex" (Graph2LaTeX.graph_to_string g false)