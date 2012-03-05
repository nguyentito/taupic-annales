let n0 = int_of_char '0'
let n9 = int_of_char '9'
let is_digit c = let n = int_of_char c in n0 <= n && n <= n9
let decode_digit c = int_of_char c - n0

let rle_decode str =
  let n = String.length str in
  let c = ref str.[0] and p = ref 0 in
  for i = 1 to n - 1 do
    let k = str.[i] in
    if is_digit k
    then p := 10 * !p + (decode_digit k)
    else (
      for j = 1 to max !p 1 do
        print_char !c;
      done;
      c := k;
      p := 0;
    )
  done;
  for j = 1 to max !p 1 do
    print_char !c;
  done;


    
