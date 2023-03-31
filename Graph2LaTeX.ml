open Graph

(* Affiche le graphe sous forme de liste d'adjacences *)
let print_graphe (graphe:Graph.graphe) =
  for i=0 to ((Array.length graphe) - 1) do
    List.iter (
      fun y ->
        print_int i;
        print_string (" --");
        print_float (y.w);
        print_string ("--> ");
        print_int (y.i);
        print_newline ();
    ) (graphe.(i))
  done

(* Une étape de descente : renvoie une nouvelle projection correspondant
   à une itération de descente de gradient, [delta] étant le pas *)
let une_descente (graphe:Graph.graphe) (proj:Maths.projection) (delta:float) =
  let e = Physics.gradient_energie graphe proj in
  (* ajout_gradient proj e (min (norme e) 0.1) *)
  Physics.ajout_gradient proj e delta



(* [n] étapes de descente. La décroissance du pas est linéaire. *)
let descente (graphe:Graph.graphe) gen (n:int) =
  let taille = Array.length graphe in
  let proj = ref (Maths.init_projection gen taille) in
  for i=1 to n do
    proj := une_descente (graphe:Graph.graphe) (!proj) (0.2 /. (1.+.Float.of_int i))
  done;
  !proj

(* Choisit la projection d'énergie minimale parmi [k] essais (différents grace à la fonction de hashage) *)
let meilleure_descente (graphe:Graph.graphe) (n:int) (k:int) =
  assert (n>0);
  assert (k>0);
  let gen = Maths.pseudo_aleatoire (Array.length graphe) in
  let meilleure_proj = ref (descente graphe gen n) in
  let min_energie = ref (Physics.energie graphe (!meilleure_proj)) in
  for i=1 to (k-1) do
  let proj = descente graphe gen n in
  let e = Physics.energie graphe proj in
    if e < !min_energie then(
      meilleure_proj := proj;
      min_energie := e 
    )
  done;
  Maths.decale (!meilleure_proj)


(* Ecrit la projection du graphe dans le fichier file *)
let write_file (file:string) (graphe:Graph.graphe) (proj:Maths.projection) =
  let oc = open_out file in
  let n = Array.length graphe in
  Printf.fprintf oc "%s" "\\begin{tikzpicture}\n";
  for i=0 to (n-1) do
    Printf.fprintf oc "%s" "\\node[draw] (S";
    Printf.fprintf oc "%s" (Int.to_string i);
    Printf.fprintf oc "%s" ") at (";
    Printf.fprintf oc "%s" (Float.to_string (proj.(i).x));
    Printf.fprintf oc "%s" ",";
    Printf.fprintf oc "%s" (Float.to_string (proj.(i).y));
    Printf.fprintf oc "%s" ") {";
    Printf.fprintf oc "%s" (Int.to_string i);
    Printf.fprintf oc "%s" "};\n";
  done;

  for i=0 to (n-1) do
    List.iter (
      fun x -> 
        Printf.fprintf oc "%s" "\\draw[->,>=latex] (S";
        Printf.fprintf oc "%s" (Int.to_string i);
        Printf.fprintf oc "%s" ") -- (S";
        Printf.fprintf oc "%s" (Int.to_string (x.i));
        Printf.fprintf oc "%s" ");\n";
    ) graphe.(i)
  done;
  Printf.fprintf oc "%s" "\\end{tikzpicture}\n\n\n"

(*let latex (graphe:Graph.graphe) (proj:Maths.projection) =
  let n = Array.length graphe in
  print_string "\\begin{tikzpicture}\n";
  for i=0 to (n-1) do
    print_string("\\node[draw] (S");
    print_int (i);
    print_string (") at (");
    print_float (proj.(i).x);
    print_string ",";
    print_float (proj.(i).y);
    print_string ") {";
    print_int i;
    print_string "};\n";
  done;

  for i=0 to (n-1) do
    List.iter (
      fun x -> 
        print_string("\\draw[->,>=latex] (S");
        print_int (i);
        print_string (") -- (S");
        print_int (x.i);
        print_string (");\n");
    ) graphe.(i)
  done;
  print_string "\\end{tikzpicture}\n\n";
  print_newline ();
  print_newline ()
  *)