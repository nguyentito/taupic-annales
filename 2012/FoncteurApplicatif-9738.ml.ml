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
  
let liste_volumes = List.map int_of_string (split_spaces (input_line stdin));;

(* liste_volumes est maintenant une "int list" qui contient la liste des volumes *)

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();;


let n = List.length liste_volumes;;
let moyenne = (List.fold_left (+) 0 liste_volumes) / n;;

let imba x = x - moyenne
let imba_pair x y = x + y - 2*moyenne



let min_imba_pair liste =
  let f (min_pair,min_val) x y =
    let value = abs (imba_pair x y) in
    if (x-moyenne)*(y-moyenne) < 0 && value < min_val then ((x,y),value) else (min_pair,min_val)
  in
  let (x0,y0) =  ((List.hd (List.filter (fun x -> x > moyenne) liste)),
                  (List.hd (List.filter (fun x -> x < moyenne) liste))) in
  List.fold_left (fun acc x -> List.fold_left (fun acc' y -> f acc' x y)
                                               acc liste)
                 ((x0,y0),imba_pair x0 y0)
                 liste

let rec remove_once x = function
  | [] -> []
  | y::ys -> if x = y then ys else y::(remove_once x ys)

let _ =
  let liste = ref (List.filter (fun x -> x <> moyenne) liste_volumes) in
  let transfer_count = ref 0 in
  while !liste <> [] do
    let ((xmin,ymin),min_val) = min_imba_pair !liste in
    liste := remove_once xmin (remove_once ymin !liste);
    if min_val <> 0 then liste := (moyenne + imba_pair xmin ymin)::!liste;
    transfer_count := !transfer_count + 1;
  done;
  affiche_solution !transfer_count;;
  
