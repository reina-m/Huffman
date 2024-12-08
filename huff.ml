let main () = 

  let in_c = open_in Sys.argv.(1) in 
  let freq_tab = Huffman.char_freq in_c in 
  Array.iteri (fun i v -> if v > 0 then Printf.printf "Char %c : %d occurences\n" (Char.chr i) v) 
    freq_tab;
  
  (*les tests sont fait avec un fichier contenant 'satisfaisant'*)
  (*tests de la construction de l'arbre*)
  let freq_heap = Huffman.freq_heap freq_tab in
  let huff_tree = Huffman.build_huff_tree freq_heap in 
  Printf.printf "Arbre de Huffman : \n";
  Huffman.print_tree huff_tree "";

  (*test des chemins de chaque charactère*)
  let codes = Huffman.chemin huff_tree in
  Printf.printf "Code de chaque char :\n";
  List.iter (fun p -> Printf.printf "Char %c : %s\n" (fst p) (snd p)) codes;
  
  close_in in_c
  (* Huffman.decompress "fichier";*)

let () = main ()