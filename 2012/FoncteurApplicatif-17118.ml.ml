(* Ce fichier contient tout ce qu'il faut pour lire les donnees en entree, les 
mettre dans des variables, et afficher votre resultat final en sortie. *)

(* Fonction pour separer les elements en entree 
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
  
let n = int_of_string (input_line stdin);;

(* recuperation du tableau des resistances resistances.(i) contient la i+1eme resistance sous la forme [|xa;ya;xb;yb|] *)
let resistances =
  let resultat = Array.make n [||] in
  for i = 0 to n-1 do
    resultat.(i) <- Array.of_list (List.map int_of_string (split_spaces (input_line stdin)));
  done;
  resultat;;

(* fonction pour afficher votre solution *)
let affiche_solution i =
  Printf.printf "%.3f" i;
  print_newline();;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let build_mat () = 
  let m = Array.make_matrix 5 5 [] in
  for i = 0 to n-1 do
    let xa = resistances.(i).(0) and ya = resistances.(i).(1) in
    let xb = resistances.(i).(2) and yb = resistances.(i).(3) in
    m.(xa).(ya) <- (1., (xb, yb))::m.(xa).(ya);
    m.(xb).(yb) <- (1., (xa, ya))::m.(xb).(yb)
  done;
  m;;

let reduire_serie m =
  let flag  = ref false in
  for i = 0 to 4 do
    for j = 0 to 4 do
      if ((i, j) <> (0, 0)) && ((i, j) <> (4,4)) then (
	if List.length m.(i).(j) = 2 then (
	  flag := true;
	  let [(r1, (xa, ya)); (r2, (xb, yb))] = m.(i).(j) in
	  if (xa, ya) <> (xb, yb) then (
	    m.(xa).(ya) <- (r1+.r2, (xb, yb))::(List.filter (fun (r, coord) -> coord <> (i, j)) m.(xa).(ya));
	    m.(xb).(yb) <- (r1+.r2, (xa, ya))::(List.filter (fun (r, coord) -> coord <> (i, j)) m.(xb).(yb));
	  ) else (
	    m.(xa).(ya) <- (List.filter (fun (r, coord) -> coord <> (i, j)) m.(xa).(ya))
	  );
	  
	  m.(i).(j) <- [];
	)
      )
    done
  done;
  !flag;;

let reduire_parallele m =
  let rec premier_doublon = function
    | [] | [_] -> None
    | (r1,p1)::(r2,p2)::q -> if p1 = p2 then Some (r1,r2,p1)
      else premier_doublon ((r2,p2)::q)
  in
  let flag = ref false in
  for i = 0 to 4 do
    for j = 0 to 4 do
      match premier_doublon m.(i).(j) with
        | None -> ()
        | Some (r1,r2,p) -> let req = (r1*.r2)/.(r1+.r2) and (x,y) = p in
                            m.(i).(j) <- (req,p)::(List.filter (fun z -> z <> (r1,p) && z <> (r2,p)) m.(i).(j));
                            m.(x).(y) <- (req,(i, j))::(List.filter (fun z -> z <> (r1,(i,j)) && z <> (r2,(i,j))) m.(x).(y));
                            flag := true;
    done
  done;
  !flag

let _ =
  let m = build_mat () in
  let flag = ref true in
  while !flag do
    let i = ref 0 in
    while reduire_parallele m do
      i := !i + 1
    done;
    while reduire_serie m do
      i := !i + 1
    done;
    flag := (!i > 0)
  done;
  affiche_solution (fst (List.hd (List.filter (fun (r, pos) -> pos = (4, 4)) m.(0).(0))));;


(* Fin de votre programme *)
