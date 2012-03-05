let char2num c = int_of_char c - int_of_char 'a'
let num2char n = char_of_int (n + int_of_char 'a')

let bigram2num c1 c2 = (char2num c1)*26 + (char2num c2)
let num2bigram n = (num2char (n/26), num2char (n mod 26))

let bigram_freq str =
  let n = String.length str in
  let t = Array.make (26*26) 0 in
  let inc i = t.(i) <- t.(i) + 1 in
  for i = 0 to n-2 do
    inc (bigram2num str.[i] str.[i+1]);
  done;
  t

let max_array t =
  let m = ref t.(0) in
  for i = 1 to Array.length t - 1 do
    if t.(i) > !m then m := t.(i);
  done;
  !m

let _ =
  Printf.printf "%d\n" (max_array (bigram_freq (input_line stdin)));
  flush(stdout)
