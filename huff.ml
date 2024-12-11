let main () = 

  Huffman.compress Sys.argv.(1)
  (* Huffman.decompress "fichier";*)

let () = main ()