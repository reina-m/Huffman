open OUnit2

(* Test de la désérialisation de l'arbre *)
let test_deserialize_tree _ =
  (* Exemple d'arbre pour le test *)
  let input_data = [1; 0; Char.code 'a'; 0; Char.code 'b'] in
  let is = Bs.of_list input_data in
  let tree = decompress.deserialize_tree is in
  assert_equal
    (Heap.Node (Heap.Leaf 'a', Heap.Leaf 'b'))
    tree

(* Test du décodage des données *)
let test_decode_tree _ =
  let tree = Heap.Node (Heap.Leaf 'a', Heap.Leaf 'b') in
  let input_data = [0; 1; 0; 0; 1; 1] in
  let is = Bs.of_list input_data in
  let result = decompress.decode_tree is tree in
  assert_equal "abba" result

(* Test de bout-en-bout *)
let test_full_decompression _ =
  (* Simule une entrée compressée et un arbre connu *)
  let input_data = [1; 0; Char.code 'a'; 0; Char.code 'b'; 0; 1; 0; 1] in
  let expected_output = "abba" in
  let compressed_file = "test_data.huff" in
  let decompressed_file = "test_data" in

  (* Crée un fichier compressé factice *)
  let oc = open_out_bin compressed_file in
  List.iter (fun bit -> output_byte oc bit) input_data;
  close_out oc;

  (* Appelle la fonction de décompression *)
  decompress decompress_file;

  (* Vérifie le contenu du fichier décompressé *)
  let ic = open_in decompressed_file in
  let output = really_input_string ic (in_channel_length ic) in
  close_in ic;

  assert_equal expected_output output

(* Regroupement des tests *)
let suite =
  "Test de l'algorithme de décompression"
  >::: [
         "test_deserialize_tree" >:: test_deserialize_tree;
         "test_decode_tree" >:: test_decode_tree;
         "test_full_decompression" >:: test_full_decompression;
       ]

let () = run_test_tt_main suite
