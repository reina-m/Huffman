type tree = 
  | Leaf of char 
  | Node of tree * tree 
;;

(*ordonnées par ordre décroissant*)
type heap = (int * tree) list
let empty = []

let is_singleton h = 
  match h with 
  | [_] -> true 
  | _ -> false 
;;

let is_empty h = 
  h = empty (*à tester*)
;;

let rec add h x = 
  match h with 
  | [] -> [x]
  | (a, _) :: tt ->
    if fst x >= a then x :: (a, _) :: tt
    else (a, _) :: add tt x
;;

(*renvoie les éléments min*)
let find_min he = 
  let rec aux acc, h = 
    match (acc, h) with 
    ([], e :: hh) -> aux [e] hh
    |((a1, b1) :: accs, (a2, b2) :: hh ) -> 
      if a2 < a1 then aux ((a2, b2)) hh
      else aux ((a2, b2) :: (a1, b1) :: accs) (*a1 = a2*)
    | (_, []) -> acc
    in
    aux [] he
;;

let remove_min h = 
;;
