open Heap
open Huffman

(* Tests pour heap *)
let test_heap () =
  assert (is_empty [||]);

  let heap = add (5, Leaf 'b') [||] in
  assert (is_singleton heap);
  assert (heap.(0) = (5, Leaf 'b'));

  let heap = add (3, Leaf 'a') heap in
  let heap = add (7, Leaf 'c') heap in
  assert (find_min heap = (3, Leaf 'a'));

  let (min1, heap') = remove_min heap in
  assert (min1 = (3, Leaf 'a'));
  let (min2, heap'') = remove_min heap' in
  assert (min2 = (5, Leaf 'b'))



(* Test pour vérifier qu'un fichier compressé puis décompressé est le même que l'original *)
let test_identique fichier =
  compress fichier;
  let fichier_compresse = fichier ^ ".hf" in
  decompress fichier_compresse;
  let fichier_decompresse = Filename.chop_suffix fichier_compresse ".hf" in

  (* on va comparer les fichiers byte par byte avec un assert pour vérifier que chaque 
  octet est identique  *)
  let ic_orig = open_in_bin fichier in
  let ic_dec = open_in_bin fichier_decompresse in
  try
    while true do
      let byte_orig = input_byte ic_orig in
      let byte_dec = input_byte ic_dec in
      assert (byte_orig = byte_dec)
    done
  with End_of_file ->
    close_in ic_orig;
    close_in ic_dec

 


let () =
  test_heap ();
  test_identique "Le_parfum_Suskind.txt";