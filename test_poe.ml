let test_huffman () =
  let original_file = "Double_Assassinat_dans_la_rue_Morgue.txt" in
  let compressed_file = original_file ^ ".hf" in
  let decompressed_file = "Double_Assassinat_dans_la_rue_Morgue_decompressed.txt" in

  (* Test de compression *)
  Printf.printf "Compression en cours pour : %s\n" original_file;
  compress original_file;
  Printf.printf "Compression terminée. Fichier compressé : %s\n" compressed_file;

  (* Test de décompression *)
  Printf.printf "Décompression en cours pour : %s\n" compressed_file;
  decompress compressed_file;
  Printf.printf "Décompression terminée. Fichier décompressé : %s\n" decompressed_file;

  (* Comparaison des fichiers *)
  Printf.printf "\nComparaison des fichiers :\n";
  Printf.printf "- Taille du fichier original : %d octets\n" (file_size original_file);
  Printf.printf "- Taille du fichier compressé : %d octets\n" (file_size compressed_file);
  Printf.printf "- Taille du fichier décompressé : %d octets\n" (file_size decompressed_file);

  (* Vérification du contenu *)
  let original_content = really_input_string (open_in original_file) (file_size original_file) in
  let decompressed_content = really_input_string (open_in decompressed_file) (file_size decompressed_file) in

  if original_content = decompressed_content then
    Printf.printf "Le contenu original et décompressé est identique. Test réussi !\n"
  else
    Printf.printf "Le contenu original et décompressé est différent. Test échoué.\n"
;;

(* Exécuter le test *)
let () = test_huffman ()
