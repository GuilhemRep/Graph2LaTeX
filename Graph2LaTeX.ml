open Graph

(* Affiche le graphe sous forme de liste d'adjacences *)
let print_graphe (graphe:Graph.graphe) =
  for i=0 to ((Array.length graphe.g) - 1) do
    List.iter (
      fun y ->
        print_int i;
        print_string (" --");
        print_float (y.w);
        print_string ("--> ");
        print_int (y.i);
        print_newline ();
    ) (graphe.g.(i))
  done

(* Une étape de descente : renvoie une nouvelle projection correspondant
   à une itération de descente de gradient, [delta] étant le pas *)
let une_descente (graphe:Graph.graphe) (delta:float) =
  let e = Physics.gradient_energie graphe in
  (* ajout_gradient proj e (min (norme e) 0.1) *)
  Physics.ajout_gradient graphe e delta



(* [n] étapes de descente. La décroissance du pas est linéaire. *)
let descente (graphe:Graph.graphe) (n:int) =
  for i=1 to n do
    une_descente graphe 0.2 (* Diminuer le pas ? *)
  done

(* Choisit la projection d'énergie minimale sur [n] étapes des descente parmi [k] essais *)
let meilleure_descente (graphe:Graph.graphe) (n:int) (k:int) =
  assert (n>0);
  assert (k>0);
  let gen = Maths.pseudo_aleatoire (Array.length graphe.g) in
  let meilleur_graphe = ref graphe in
  descente graphe n;
  let min_energie = ref (Physics.energie graphe) in
  for i=1 to (k-1) do
    Graph.shake graphe gen;
    descente graphe n;
    let e = Physics.energie graphe in
      if e < !min_energie then(
        meilleur_graphe := graphe;
        min_energie := e 
      )
  done;
  graphe.p<- (Graph.decale (!meilleur_graphe)).p


(* Ecrit la projection du graphe dans le fichier file, w a vrai si on veut les poids *)
let graph_to_string (graphe:Graph.graphe) (w:bool)=
  let str = ref "" in
  let n = Array.length graphe.g in
  str := !str ^ "\\begin{tikzpicture}[
    vertex/.style = {shape=rectangle,draw,minimum size=1em},
    edge/.style = {-,-Latex},
    ]\n";

  

  for i=0 to (n-1) do
    str := !str ^ "\\node[vertex] (S";
    str := !str ^ (Int.to_string i);
    str := !str ^ ") at (";
    str := !str ^ (Float.to_string (graphe.p.(i).x));
    str := !str ^ ",";
    str := !str ^ (Float.to_string (graphe.p.(i).y));
    str := !str ^ ") {";
    str := !str ^ (graphe.l.(i));
    str := !str ^ "};\n";
  done;

  for i=0 to (n-1) do
    List.iter (
      fun x -> 
        str := !str ^ "\\draw[edge] (S";
        str := !str ^ (Int.to_string i);
        str := !str ^ ") to node[above,sloped] ";
        str := !str ^ "{$";
        if w then str := !str ^ (Float.to_string (x.w));
        str := !str ^ "$} ";
        str := !str ^ "(S";
        str := !str ^ (Int.to_string (x.i));
        str := !str ^ ");\n";
    ) graphe.g.(i)
  done;  

  str := !str ^ "\\end{tikzpicture}\n\n\n";
  !str

(* Ecrit la projection du graphe dans le fichier file *)
let write_file (file:string) (graphe:Graph.graphe) (w:bool) =
  let oc = open_out file in
  Printf.fprintf oc "%s" (graph_to_string graphe w)

(* Ecrit str dans file *)
let write_string_file (file:string) (str:string) =
  let oc = open_out file in
  Printf.fprintf oc "%s" str