let main () =
  match Sys.argv with
  | [| _; "--help" |] -> print_help ()
  | [| _; fichier |] when Filename.check_suffix fichier ".hf" -> decompress fichier
  | [| _; fichier |] ->
      Printf.printf "Compression du fichier : %s\n" fichier;
      compress fichier
  | [| _; "--stats"; fichier |] -> stats fichier
  | _ ->
      Printf.printf "Fichier introuvable.\n";
      print_help ()
;;

(* Lancer le programme *)
let () = main ()