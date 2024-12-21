let input_code cin = 
  (*fonction qui gère l'exception*)
  try 
    input_byte cin 
  with End_of_file -> -1
;;


let char_freq in_c = 
  let tab = Array.make 256 0 in
  let rec loop () = 
        let o = input_code in_c in 
          if o < 0 then tab
          else (
            tab.(o) <- tab.(o) + 1;
            loop ()
          )
  in
  loop ()
;;

(*contruire le nombre d'occurences de chaque caractère du texte
par exemple : H = (3, s) (3, a) (2, t) (2, i) (1, f) (1, n) *)

let freq_heap tab = 
  let rec aux acc i = 
    if i = 256 then acc
    else 
      let nacc = 
      if tab.(i) > 0 then (tab.(i), Heap.Leaf (Char.chr i)) :: acc 
      else acc 
      in 
      aux nacc (i+1)
  in
  aux [] 0
;;

(*construire l'arbre à partir de heap de fréquence*)
let build_huff_tree h = 
  let rec loop heap = 
    match heap with 
    | [] -> failwith "build_huff_tree on empty heap"
    | [(_, t)] -> t (*il reste un arbre au final*)
    | _ -> (*au moins deux*)
      (*extraire les deux minimums*)
      let (f1, t1), heap1 = Heap.remove_min heap in 
      let (f2, t2), heap2 = Heap.remove_min heap1 in 
      (*combiner ces deux minimums*)
      let t3 = Heap.Node (t1, t2) in 
      let heap3 = Heap.add ((f1 + f2), t3) heap2 in 
      loop heap3
  in
  loop (List.sort (fun a b -> compare (fst a) (fst b)) h)
;;

(*fonction qui affiche l'arbre, ajoutée pour tests, i accumulateur pour l'identation*)
let rec print_tree t i = 
  match t with 
  | Heap.Leaf c -> Printf.printf "%sLeaf '%c'\n" i c
  | Heap.Node (g, d) -> (*gauche et droit*)
    Printf.printf "%sNode\n" i;
    print_tree g (i ^ " ");
    print_tree d (i ^ " ")
;;

(*FONCTIONS POUR GESTION DE LA LIGNE DE COMMANDE :*)

(*fonction qui affiche le message d'aide*)
let help () = 
  Printf.printf "=============================================\n";
  Printf.printf "              Programme Huff                 \n";
  Printf.printf "=============================================\n\n";

  Printf.printf "huff --help              Affiche le message d'aide\n";
  Printf.printf "huff fichier             Compression du fichier\n";
  Printf.printf "huff fichier.hf          Décompression du fichier\n";
  Printf.printf "huff --stats fichier     Compression et statistques\n";

  Printf.printf "\n";
  Printf.printf "=============================================\n\n"
;;

let stats () = failwith "todo"

(*fonction qui donne le code (chemin) des caractères à partir de l'arbre de huffman
droite : 1, gauche : 0
renvoie une liste de paire dont la première composante est le caractère
la seconde est son code *)
let code_of_tree tree =
  let rec loop t code acc = 
    match t with 
    | Heap.Leaf c -> (c, code) :: acc
    | Heap.Node (g, d) -> 
      let nacc = loop g (code ^ "0") acc in 
      loop d (code ^ "1") nacc
  in
  loop tree "" []
;;

(*fonction pour sérialisation de l'arbre
si on est dans un noeud interne, on écrit 1 puis sur SAG et SAD
si on est dans une feuille, on écrit 0 *)
let rec serialize_tree os t = 
  match t with 
  | Heap.Leaf c -> 
    Bs.write_bit os 0; 
    Bs.write_byte os (Char.code c);
  | Heap.Node (g, d) -> 
    Bs.write_bit os 1;
    serialize_tree os g; 
    serialize_tree os d
;;

(*pour écrire le ostream dans un fichier compressé
à partir d'un istream et la liste de codes (qui est comme un dictionnaire)*)
let write_data in_c codes os =
  let rec loop () =
    match input_code in_c with
    | -1 -> () (* eof *)
    | bit ->
      let code = List.assoc (Char.chr bit) codes in
      Bs.write_n_bits os (String.length code) (int_of_string ("0b" ^ code));
      loop ()
  in
  loop ()
;;

let compress f =
  let in_c = open_in f in
  let freq_tab = char_freq in_c in
  let freq_heap = freq_heap freq_tab in
  let huff_tree = build_huff_tree freq_heap in
  let char_codes = code_of_tree huff_tree in

  let f2 = f^".hf" in 
  let cout = open_out f2 in
  let os = Bs.of_out_channel cout in
  serialize_tree os huff_tree;

  seek_in in_c 0;
  write_data in_c char_codes os;

  Bs.finalize os;
  close_in in_c;
  close_out cout
;;

(*prend en argument une séquence binaire*)
let rec deserialize_tree b = 
  match Bs.read_n_bits b 1 with 
  | 0 -> (*lit une feuille puis son code*)
    let char = Bs.read_n_bits b 8 in 
    Heap.Leaf (Char.chr char) (*codé sur 8 bits*)
  | 1 -> 
    let g = deserialize_tree b in 
    let d = deserialize_tree b in 
    Heap.Node (g, d)
  | _ -> failwith "desrialize tree : bit invalide"
;;

(*décode une séquence binaire, prend la séquence et l'arbre de huffman en arguments*)
let rec decode_data b t = 
  let rec loop bits tree acc = 
    match bits, tree with 
    | [], _ -> List.rev acc (*il ne reste plus de bits*)
    | 0 :: ll, Heap.Node (g, _) -> loop ll g acc
    | 1 :: ll, Heap.Node (_, d) -> loop ll d acc
    (*on ajoute le charactère dans la liste*)
    | _, Heap.Leaf char -> loop bits t (char :: acc)
    | _ -> failwith "decode_data : bit invalide"
  in 
  loop b t []
;;

let rec read_bits b = 
  try 
    let bit = Bs.read_n_bits b 1 in 
    bit :: read_bits b
  with 
  | Bs.End_of_stream -> []
;;

let decompress f = 
  let in_c = open_in f in 
  let b = Bs.of_in_channel in_c in 
  let huff_tree = deserialize_tree b in 
  (*les bits qui restent (sans l'arbre)*)
  let data = decode_data (read_bits b) huff_tree in 
  let res = Filename.chop_suffix f ".hf"^"_decompressed" in 
  let cout = open_out res in 
  List.iter (fun char -> output_byte cout (Char.code char)) data;
  close_out cout; 
  close_in in_c
;;
