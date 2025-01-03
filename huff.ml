let main () =
  if Array.length Sys.argv < 2 then (
    Huffman.help ();
    exit 1
  ) else
    match Sys.argv.(1) with
    | "--help" -> Huffman.help ()
    | "--stats" ->
      if Array.length Sys.argv < 3 then (
        Printf.eprintf "Erreur : fichier non spécifié pour --stats\n";
        exit 1
      ) else if not (Sys.file_exists Sys.argv.(2)) then (
        Printf.eprintf "Erreur : fichier %s introuvable\n" Sys.argv.(2);
        exit 1
      ) else
        Huffman.stats Sys.argv.(2)
    | file when Filename.check_suffix file ".hf" ->
      if not (Sys.file_exists file) then (
        Printf.eprintf "Erreur : fichier %s introuvable\n" file;
        exit 1
      ) else (
        Printf.printf "Décompression du fichier %s\n" file;
        try Huffman.decompress file
        with e -> Printf.eprintf "Erreur de décompression : %s\n" (Printexc.to_string e)
      )
    | file ->
      if not (Sys.file_exists file) then (
        Printf.eprintf "Erreur : fichier %s introuvable\n" file;
        exit 1
      ) else (
        Printf.printf "Compression du fichier %s\n" file;
        try Huffman.compress file
        with e -> Printf.eprintf "Erreur de compression : %s\n" (Printexc.to_string e)
      )
;;


let () = main ()