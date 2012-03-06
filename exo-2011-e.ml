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

(* recuperation du nombre d'allumettes *)
let allumettes = int_of_string (input_line stdin);;

(* recuperation de la liste des coups possibles *)
let liste_coups =
  List.map int_of_string (split_spaces (input_line stdin));;

(* Le nombre d'allumettes initial et la liste des coups
possibles sont maintenant definis *)

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();
  flush(stdout);;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)


let position_gagnante_active  = Array.make (allumettes+1) false (* perdante passive *)
let position_gagnante_passive = Array.make (allumettes+1) false (* perdante active *)

let calculer () =
  let m = List.fold_left max 0 liste_coups in
  position_gagnante_passive.(0) <- true;
  position_gagnante_active.(0) <- false;
  for i = 1 to m do
    position_gagnante_active.(i) <- true;
    position_gagnante_passive.(i) <- false;
  done;
  for i = m+1 to allumettes do
    position_gagnante_active.(i) <- 
      List.exists (fun x -> position_gagnante_passive.(i-x))
                  liste_coups;
    position_gagnante_passive.(i) <-
      List.for_all (fun x -> position_gagnante_active.(i-x))
                   liste_coups;
  done;
  List.fold_left (+) 0 (List.filter (fun y -> position_gagnante_passive.(allumettes-y))
                                    liste_coups)

let _ = affiche_solution (calculer ())
