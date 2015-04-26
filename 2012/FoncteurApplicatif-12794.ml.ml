(* Ce fichier contient tout ce qu'il faut pour lire les donnees en entree, les 
mettre dans des variables, et afficher votre resultat final en sortie. *)

type expr = 
  | Op of int
  | Nop of int
  | Et of expr * expr
  | Ou of expr * expr
  | Non of expr;;

(* Fonction pour lire l'entree et la representer dans le format de votre choix --- a completer --- *)
let rec lit_entree () = 
  let ligne = input_line stdin in
  if ligne <> "" then (
    let i = 
      try int_of_string ligne with _ -> 0
    in
    if i = 0 then (
      if ligne = "non" then Non (lit_entree ())
      else if ligne = "et" then Et (lit_entree (), lit_entree())
      else (* ligne = "ou" *) Ou (lit_entree (), lit_entree())
    ) else (
	Op i
    )
  ) else failwith "err"
;;

(* fonction pour afficher votre solution *)
let affiche_solution b =
  if b then print_string "true" else print_string "false";
  print_newline();;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let rec eliminer_doublons = function
  | [] -> []
  | x::x'::xs when x=x' -> eliminer_doublons (x::xs)
  | x::xs -> x::(eliminer_doublons xs)

let rec nier = function
  | Op i -> Nop i
  | Nop i -> Op i
  | Et (e1,e2) -> Ou (nier e1, nier e2)
  | Ou (e1,e2) -> Et (nier e1, nier e2)
  | Non e1 -> virer_nons e1
and virer_nons = function
  | Op i -> Op i
  | Nop i -> Nop i
  | Et (e1,e2) -> Et (virer_nons e1, virer_nons e2)
  | Ou (e1,e2) -> Ou (virer_nons e1, virer_nons e2)
  | Non e1 -> nier e1


let rec pos_sat current_list = function
  | Op i -> List.map (fun t -> t.(i) <- Some true; t)
                     (List.filter (fun t -> t.(i) <> Some false) current_list)
  | Nop i -> List.map (fun t -> t.(i) <- Some false; t)
                     (List.filter (fun t -> t.(i) <> Some true) current_list)
  | Et (a,b) -> pos_sat (pos_sat current_list a) b
  | Ou (a,b) -> let other_list = List.map Array.copy current_list in
		let after_a = pos_sat current_list a
		and after_b = pos_sat other_list b in
		after_a @ after_b
  | Non expr -> failwith "fuck that shit"

let _ = affiche_solution (pos_sat [Array.make 40 None] (virer_nons (lit_entree ())) <> [])

  

(* Fin de votre programme *)
