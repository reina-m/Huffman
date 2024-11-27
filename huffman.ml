let decompress _ = failwith "todo"
let compress _ = failwith "todo"

let rec char_freq in_c = 
  let tab = Array.make 256 0 in
    try 
      let o = input_byte in_c in 
      tab.(o) <- tab.(o) + 1;
      char_freq in_c
    with 
    | End_of_file -> tab
;;