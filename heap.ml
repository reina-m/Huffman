type tree = 
  | Leaf of char 
  | Node of tree * tree 
;;

(*ordonnées par ordre croissant*)
type heap = (int * tree) list
let empty = []

let is_singleton h = 
  match h with 
  | [_] -> true 
  | _ -> false 
;;

let is_empty h = 
  h = empty
;;

let rec add h x = 
  match h with 
  | [] -> [x]
  | (a, b) :: hh ->
    if fst x <= a then x :: h
    else (a, b) :: add hh x
;;

let find_min he = 
  match h with 
  | [] -> failwith "find_min on empty heap"
  | e :: _ -> e
;;

let remove_min h = 
  match h with 
  | [] -> failwith "find_min on empty heap"
  | e :: hh -> (e, hh)
;;
