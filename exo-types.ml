open Printf
open Scanf

(*** Exercice 1 ***)

let resoudre_exo1 t0 t1 c1 c2 c =
  let t = ref t0 and t' = ref t1 and i = ref 0 in
  while !t' <= 100000000 do
    i := !i + 1;
    let tmp = !t' in
    t' := c1 * !t' + c2 * !t + c;
    t := tmp;
  done;
  !i

let executer_exo1 () =
  printf "%d\n" (scanf "%d %d %d %d %d" resoudre_exo1)


(*** Exo 2 ***)

(** Contenu du fichier squelette **)

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
  !res

let parse_input () =
  let n,d,a = 
    match List.map int_of_string (split_spaces (input_line stdin)) with
      |[n;d;a]->n,d,a
      |_-> failwith "Mauvais format d'entree"
  in
  let g = Array.make_matrix n n false in
  let rec store_line i j remaining_entries =
    match remaining_entries with
      | [] -> ()
      | head::tail ->
          g.(i).(j) <- (head = "1");
          store_line i (j+1) tail
  in
  try
    for i=0 to n-1 do
      let line = split_spaces (input_line stdin) in
        store_line i 0 line
    done;
    (n,d,a,g)
  with 
    |_->failwith "Mauvais format d'entree"

let bool_mat_mult a b =
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

let resoudre_exo2 (n,d,a,g) =
  let i = ref 0 and mat_adj = ref g in
  while !i < n && not (!mat_adj.(d).(a)) do
    i := !i + 1;
    mat_adj := bool_mat_mult !mat_adj g;
  done;
  !mat_adj.(d).(a)

let print_bool x = print_string (if x then "true" else "false")

let executer_exo2 () =
  print_bool (resoudre_exo2 (parse_input ()));
  print_newline();
  flush(stdout) 
  

         
