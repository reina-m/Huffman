open Heap


let input_code cin = 
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
  Array.fold_left (fun heap (i, freq) ->
    if freq > 0 then add (freq, Leaf (Char.chr i)) heap else heap
  ) empty (Array.mapi (fun i freq -> (i, freq)) tab)
;;
  
(*construire l'arbre à partir de heap de fréquence*)
let build_huff_tree h = 
  let rec loop heap = 
    if is_empty heap then failwith "build_huff_tree on empty heap"
    else if is_singleton heap then snd (find_min heap)  (* Un seul élément, c'est l'arbre final *)
    else (
      (* Extraire les deux éléments minimaux *)
      let (f1, t1), heap1 = remove_min heap in
      let (f2, t2), heap2 = remove_min heap1 in
      (* Combiner ces deux éléments *)
      let t3 = Node (t1, t2) in
      let heap3 = add (f1 + f2, t3) heap2 in
      loop heap3
    )
  in
  loop h

(*fonction qui affiche l'arbre, ajoutée pour tests, i accumulateur pour l'identation*)
let rec print_tree t i = 
  match t with 
  | Leaf c -> Printf.printf "%sLeaf '%c'\n" i c
  | Node (g, d) -> (*gauche et droit*)
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

(* Fonction pour calculer la taille d'un fichier *)
let file_size f =
  let i = open_in_bin f in
  let size = in_channel_length i in
  close_in i;
  size
;;




(*fonction qui donne le code (chemin) des caractères à partir de l'arbre de huffman
droite : 1, gauche : 0
renvoie une liste de paire dont la première composante est le caractère
la seconde est son code *)
let code_of_tree tree =
  let rec loop t code acc = 
    match t with 
    | Leaf c -> (c, code) :: acc
    | Node (g, d) -> 
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
  | Leaf c -> 
    Bs.write_bit os 0; 
    Bs.write_byte os (Char.code c);
  | Node (g, d) -> 
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

(* Fonction pour afficher les statistiques de compression *)
let stats fichier =
  let compressed_file = fichier ^ ".hf" in
    Printf.printf "Compression en cours pour le fichier : %s\n" fichier;
    compress fichier;
    Printf.printf "Compression terminée.\n";
    let original_size = file_size fichier in
    let compressed_size = file_size compressed_file in

    let compression_ratio =
      if original_size = 0 then 0.0
      else (1.0 -. (float_of_int compressed_size /. float_of_int original_size)) *. 100.0
    in
    Printf.printf "Statistiques de compression :\n";
    Printf.printf "Taille originale : %d octets\n" original_size;
    Printf.printf "Taille compressée : %d octets\n" compressed_size;
    Printf.printf "Taux de compression : %.2f%%\n" compression_ratio
;;


(* Fonction pour reconstruire l'arbre de Huffman à partir d'un flux binaire *)
let rec deserialize_tree is =
    match Bs.read_bit is with
    | 0 -> 
      let c = Char.chr (Bs.read_byte is) in
      Leaf c
    | 1 -> 
      let left = deserialize_tree is in
      let right = deserialize_tree is in
      Node (left, right)
    | _ -> raise (Failure "deserialize_tree: Invalid bit")





(* Fonction principale pour la décompression *)
let decompress f =

    Printf.printf "Décompression du fichier %s\n" f;
    let cin = open_in f in
    let is = Bs.of_in_channel cin in
    let huff_tree = deserialize_tree is in

    (* ici je cherche a decoder en utilisant une boucle iterative *)
    (* il y a aussi l'utilisation d'un Buffer pour pouvoir 
    apprivoiser les gros fichiers*)
    let decode_tree_iterative is tree =
      let result = Buffer.create 1024 in
      let rec loop node =
        match node with
        | Leaf c ->
          Buffer.add_char result c;
          loop tree (* Revenir à la racine après avoir trouvé un caractère *)
        | Node (left, right) -> (
            match Bs.read_bit is with
            | 0 -> loop left
            | 1 -> loop right
            | _ -> failwith "decode_tree: Invalid bit")
      in
      (try loop tree with Bs.End_of_stream -> ());
      Buffer.contents result
    in

    let data = decode_tree_iterative is huff_tree in
    let f2 = Filename.chop_suffix f ".hf" in

    (* Enregistrer les données décompressées *)
    let cout = open_out f2 in
    output_string cout data;
    close_in cin;
    close_out cout;

    Printf.printf "Fichier décompressé enregistré sous : %s\n" f2

  
