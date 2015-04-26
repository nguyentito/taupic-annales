(* Ce fichier contient tout ce qu'il faut pour lire les donnees en entree, les
mettre dans des variables, et afficher votre resultat final en sortie. *)

(* on recupere c1 et c2, les chaines de caractere fonctionnent comme les tableaux
en caml-light et OCaml : on accede a un element en i-eme position d'une chaine c
   en utilisant c.[i] en OCaml ou c.(i) en CamlLight, et a la taille de c en utilisant
string_length c en CamlLight ou String.length c en OCaml *)
let c1 = input_line stdin;;
let c2 = input_line stdin;;

(* fonction pour afficher votre solution *)
let affiche_solution i =
  print_int i;
  print_newline();;

(* Mettez apres ceci le corps de votre programme *)
(* N'oubliez pas de terminer par un appel a affiche_solution !*)

(* Debut de votre programme *)

let compare c1 c2 i n2 =
  let b = ref true in
  for j = 0 to n2-1 do
    if c1.[i+j] <> c2.[j] then b := false
  done;
  !b;;

let () =
  let n1 = String.length c1 and n2 = String.length c2 in
  let count = ref 0 in
  let i = ref 0 in
  while !i <= n1 - n2  do
    if compare c1 c2 !i n2 then (count := !count + 1;
				  i := !i + n2)
    else i := !i + 1
  done;
  affiche_solution !count;;

(* Fin de votre programme *)
