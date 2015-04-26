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
  
let coordonnees = 
  let rec aux = function
    | [] -> []
    | [x] -> failwith "OH NOES"
    | x::y::ys -> (x, y)::(aux ys) in
  let xa::ya::xs = (List.map float_of_string (split_spaces (input_line stdin))) in
  (aux (xa::ya::xs))@[(xa, ya)];;


(* Les coordonnees sont maintenant connues ! *)

let rectangle_encadrant () =
  let xmin = List.fold_left (fun acc x -> if (fst x) < acc then fst x else acc) 5000000. coordonnees in
  let xmax = List.fold_left (fun acc x -> if (fst x) > acc then fst x else acc) 0. coordonnees in
  let ymin = List.fold_left (fun acc x -> if (snd x) < acc then snd x else acc) 5000000. coordonnees in
  let ymax = List.fold_left (fun acc x -> if (snd x) > acc then snd x else acc) 0. coordonnees in
  ((xmin, ymin), (xmax, ymax));;

let eps = 0.01;;

let satisfait (a, b, c) (x, y) =
  a*.x +. b*.y +. c >= 0.;;

let rec paires = function
  | [] | [_] -> []
  | x::x'::xs -> (x,x')::(paires (x'::xs))

let demi_plan arete = let ((xa,ya),(xb,yb)) = arete in
                      let (xn,yn) = (yb-.ya, xa-.xb) in
                      let c = (xn*.xa +. yn*.ya) in
                      (xn,yn,-.c)

let demi_plans sommets = List.map demi_plan (paires sommets)

let demi_plans_l = demi_plans coordonnees;;

(* fonction pour afficher votre solution *)
let affiche_solution i =
  if i then print_string "true" else print_string "false";
  print_newline();;



(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

let _ =
  let trouve = ref false in
  let ((xmin, ymin), (xmax, ymax)) = rectangle_encadrant () in
  let x = ref xmin and y = ref ymin in
try
  while !y <= ymax do
    while !x <= xmax do
      if (List.fold_left (fun acc dp -> acc && (satisfait dp (!x, !y))) true demi_plans_l) then
	raise (Failure "Succes");
      x := !x +. eps
    done;
    y := !y +. eps;
    x := xmin
  done;
  affiche_solution false
with Failure s -> affiche_solution true
