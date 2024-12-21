let main () = 

  let f = Sys.argv.(1) in 
  Huffman.compress f;
  Huffman.decompress (f ^ ".hf")

let () = main ()