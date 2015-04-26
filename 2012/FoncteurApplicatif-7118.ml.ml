
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
  
(* On lit la liste des pièces de monnaie disponibles *)
let pieces = List.sort compare (List.map int_of_string (split_spaces (input_line stdin)))

(* On lit la somme a atteindre, en cents, et on la stocke dans un entier n *)
let n = int_of_string (input_line stdin);;

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let rec nombre_combis k  = function
  | _ when k = 0 -> 1
  | _ when k < 0 -> 0
  | p::ps -> if ps = [] then nombre_combis (k-p) (p::ps)
             else nombre_combis (k-p) (p::ps) + nombre_combis k ps
  | [] -> 0

let _ = affiche_solution (nombre_combis n pieces );;

  
