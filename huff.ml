let main () = 

  let in_c = open_in Sys.argv.(1) in 
  let freq_tab = Huffman.char_freq in_c in 
  Array.iteri (fun i v -> if v > 0 then Printf.printf "Char %c : %d occurences\n" (Char.chr i) v) 
    freq_tab;
  
  (*tests de la construction de l'arbre*)
  let freq_heap = Huffman.freq_heap in_c in
  let huff_tree = Huffman.build_huff_tree freq_heap in 
  Printf.printf "Arbre de Huffman : \n";
  Huffman.print_tree huff_tree "";
  
  close_in in_c
  (* Huffman.decompress "fichier";*)

let () = main ()