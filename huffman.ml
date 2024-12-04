let decompress _ = failwith "todo"
let compress _ = failwith "todo"

let input_code cin = 
  (*fonction qui gère l'exception*)
  try 
    input_byte cin 
  with End_of_file -> -1
;;


let char_freq in_c = 
  let tab = Array.make 256 0 in
  let rec loop () = 
        let o = input_code in_c in 
          if o < 0 then tab
          else (
            tab.(o) <- tab.(o) + 1;
            loop ()
          )
  in
  loop ()
;;

(*contruire le nombre d'occurences de chaque caractère du texte
par exemple : H = (3, s) (3, a) (2, t) (2, i) (1, f) (1, n) *)

let freq_heap in_c = 
  let tab = char_freq in_c in 
  let rec aux acc i = 
    match i with 
      256 -> acc
      | _ -> aux ((tab.(i),  Char.chr i) :: acc) (i+1)
  in
  (aux [] 0)
;;
