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
            aux ()
          )
  in
  loop ()
;;