(* fonction pour separer les elements en entree 
Il n'est pas necessaire de comprendre cette fonction pour utiliser le programme !*)
    let split_spaces chaine =
  if chaine = ""
  then []
  else 
    let res = ref [] in
    let temp = ref "" in
  for i = 0 to String.length chaine - 1 do
    if chaine.[i]=' '
    then 
      begin
        res := !res @ [!temp];
        temp := "";
      end
    else
      temp := !temp ^ (String.sub chaine i 1);
  done;
  res := !res @ [!temp];
  !res;;

(* recuperation du nombre de neurones *)
let nombre_neurones = int_of_string (input_line stdin);;

(* recuperation de la structure du reseau en tableau de listes *)
let reseau =
  let tableau = Array.make nombre_neurones [] in
  for i = 0 to Array.length tableau - 1 do
    tableau.(i) <- List.map int_of_string (split_spaces (input_line stdin))
  done;
  tableau;;
    
(* Le nombre de neurones et le reseau sont maintenant definis *)

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();
  flush(stdout);;


let mult_mat_bin a b =
  let n = Array.length a in
  let c = Array.make_matrix n n false in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
        c.(i).(j) <- c.(i).(j) || (a.(i).(k) && b.(k).(j));
      done;
    done;
  done;
  c;;

let exists_cycle mat_adj =
  let b = ref false in
  for i = 0 to Array.length mat_adj - 1 do
    b := !b || mat_adj.(i).(i);
  done;
  !b;;

let resoudre () =
  let reseau_mat = Array.make_matrix nombre_neurones nombre_neurones false in
  for i = 0 to nombre_neurones - 1 do
    List.iter (fun j -> reseau_mat.(i).(j) <- true) reseau.(i);
  done;
  let mat_adj = ref reseau_mat and path_length = ref 1 in
  while not (exists_cycle !mat_adj) do
    path_length := !path_length + 1;
    mat_adj := mult_mat_bin !mat_adj reseau_mat;
  done;
  affiche_solution !path_length;;

let _ = resoudre ();;

