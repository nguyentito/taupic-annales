
(* On lit la somme a atteindre, en cents, et on la stocke dans un entier n *)
let u1 = int_of_string (input_line stdin);;
let u2 = int_of_string (input_line stdin);;
let n = int_of_string (input_line stdin);;

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let tab = Array.make (n+1) 0;;

let _ =
  tab.(1) <- u1;
  for i = 2 to 1+u1 do
    tab.(i) <- u2
  done;
  let i = ref 2 and j = ref (u1+2) in
  while !j <= n do
    tab.(!j) <- u1;
      j := !j + 1;
      let k = ref 1 in
    while !k <= tab.(!i) && !j <= n do
      tab.(!j) <- u2;
      j := !j + 1;
      k := !k +1;
    done;
  done;
  affiche_solution tab.(n)

    
  
  


(* Fin de votre programme *)
