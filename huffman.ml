let decompress _ = failwith "todo"
let compress _ = failwith "todo"

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
let chemin tree =
  let rec loop t code acc = 
    match t with 
    | Heap.Leaf c -> (c, code) :: acc
    | Heap.Node (g, d) -> 
      let nacc = loop g (code ^ "0") acc in 
      loop d (code ^ "1") nacc
  in
  loop tree "" []
;;