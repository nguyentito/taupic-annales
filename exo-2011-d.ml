let string_to_list str =
  let l = ref [] in
  for i = String.length str - 1 downto 0 do
    l := (int_of_char str.[i]) :: !l;
  done;
  !l

let rec pta_size = function
  | [] -> 0
  | [[]] -> 1
  | word_list ->
      let s = ref 1 in
      for i = int_of_char 'a' to int_of_char 'z' do
        let l = List.filter (fun x -> x <> [] && List.hd x = i) word_list in
        if l <> [] then s := !s + pta_size (List.map List.tl l)
      done;
      !s

let get_l_pos () =
  let rec liste () = 
    try
      let mot = input_line stdin
      in
        mot::liste()
    with
      | End_of_file -> []
  in
    liste ();;

let _ = Printf.printf "%d\n" (pta_size (List.map string_to_list (get_l_pos ())))
