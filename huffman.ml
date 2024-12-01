let decompress _ = failwith "todo"
let compress _ = failwith "todo"

let char_freq in_c = 
  let tab = Array.make 256 0 in
  let rec aux () = 
      try 
        let o = input_byte in_c in 
        tab.(o) <- tab.(o) + 1;
        aux ()
      with 
      | End_of_file -> tab
    in
    aux ()
;;