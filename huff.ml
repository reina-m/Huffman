open Huffman

let main () =
  let argc = Array.length Sys.argv in
  if argc = 2 then
    let arg = Sys.argv.(1) in
    if arg = "--help" then help ()
    else if Filename.check_suffix arg ".hf" then decompress arg
    else (
      Printf.printf "Compression du fichier : %s\n" arg;
      compress arg
    )
  else if argc = 3 && Sys.argv.(1) = "--stats" then
    stats Sys.argv.(2)
  else (
    Printf.printf "Usage pas bon. Utilisez '--help' pour plus avoir plus d'information ;)\n";
    help ()
  )
;;
let () = main ()
