(* Ce fichier contient tout ce qu'il faut pour lire les donnees en entree, les 
mettre dans des variables, et afficher votre resultat final en sortie. *)

(* fonction pour separer les elements en entree 
Il n'est pas necessaire de comprendre cette fonction pour utiliser le programme !*)
    let split_spaces chaine =
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

(* recuperation des parametres n et k *)
let (n,k) = 
  match List.map int_of_string (split_spaces (input_line stdin)) with
    |[n;k]->(n,k)
    |_->failwith "Mauvais format d'entree";;

(* recuperation de la matrice G qui contient n lignes et k colonnes *)
let matriceG =
  let resultat = Array.make n [||] in
  for i = 0 to n-1 do
    resultat.(i) <- Array.of_list (List.map int_of_string (split_spaces (input_line stdin)));
  done;
  resultat;;

(* La matrice G, n et k sont maintenant definis *)

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();
  flush(stdout);;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let write_binary_decomposition n t =
  let m = ref n and i = ref 0 in
  while !m > 0 do
    t.(!i) <- !m mod 2;
    m := !m / 2;
    i := !i + 1;
  done;
  for j = !i to Array.length t - 1 do
    t.(j) <- 0;
  done

let mult_mat_vect g v =
  let n = Array.length g and k = Array.length v in
  let w = Array.make n 0 in
  for i = 0 to n-1 do
    for j = 0 to k-1 do
      w.(i) <- (w.(i) + g.(i).(j) * v.(j)) mod 2;
    done;
  done;
  w
  
let count_ones w =
  let c = ref 0 in
  for i = 0 to Array.length w - 1 do
    c := !c + w.(i);
  done;
  !c

let rec power_of_two = function
  | 0 -> 1
  | p -> 2*(power_of_two (p-1))
  
let minimal_code_distance n k g =
  let v = Array.make k 0 in
  v.(0) <- 1;
  let c = ref (count_ones (mult_mat_vect g v)) in
  for i = 2 to (power_of_two k) - 1 do
    write_binary_decomposition i v;
    c := min !c (count_ones (mult_mat_vect g v));
  done;
  !c

let _ = affiche_solution (minimal_code_distance n k matriceG)

(* Fin de votre programme *)
