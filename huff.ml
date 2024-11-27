let main () = 
  let in_c = open_in Sys.argv.(1) in 
  let tab = Huffman.char_freq in_c in 
  Array.iteri (fun i v -> Printf.printf "Octet %d : %d occurences\n" i v) tab

  (* Huffman.decompress "fichier";*)

let () = main ()