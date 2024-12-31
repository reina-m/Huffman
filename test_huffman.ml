open Huffman

(* Fonction de test de compression et décompression *)
let test_compression_decompression () =
  let input_file = "test_input.ml" in
  let compressed_file = "test_input.ml.hf" in
  let decompressed_file = "decompressed_output.txt" in

  (* Étape 1: Compression du fichier *)
  print_endline "Compression en cours...";
  Huffman.compress input_file;
  print_endline "Compression terminée.";

  (* Étape 2: Décompression du fichier *)
  print_endline "Décompression en cours...";
  Huffman.decompress compressed_file;
  print_endline "Décompression terminée.";

  (* Vérification des résultats *)
  (* Comparer le fichier d'entrée avec le fichier décompressé *)
  let input_data = 
    let ic = open_in input_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in

  let decompressed_data = 
    let ic = open_in decompressed_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in

  (* Vérification : Si les fichiers sont identiques *)
  if input_data = decompressed_data then
    print_endline "Test réussi : La décompression est correcte."
  else
    print_endline "Test échoué : Les données ne correspondent pas."

(* Exécuter le test *)
let () = test_compression_decompression ()
