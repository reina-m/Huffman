type tree = 
  | Leaf of char 
  | Node of tree * tree 
;;

type t = (int * tree) list

let empty = []

let is_singleton h = 
  match h with 
  | [_] -> true 
  | _ -> false 
;;

let is_empty h = 
  h = empty
;;

let rec remonte_heap h i =
  if i > 0 then 
    let parent_index = (i - 1) / 2 in (* formule pour avoir l'indice du parent situe a l'indice i *)
    let parent = List.nth h parent_index in 
    if fst (List.nth h i) < fst parent then
      let tmp = List.nth h i in
      let h' = List.mapi (fun idx elem -> 
        if idx = i then parent
        else if idx = parent_index then tmp
        else elem
      ) h in
      remonte_heap h' parent_index
    else 
      h 
  else 
    h


let rec add x h = 
  let h' = h @ [x] in 
  let rec aux heap i = 
    if i < List.length heap then
      let h2 = remonte_heap heap i in
      aux h2 (i + 1)
    else 
      heap
  in
  aux h' (List.length h')

let find_min h = 
  match h with 
  | [] -> failwith "find_min on empty heap"
  | e :: _ -> e
;;

let remove_min h = 
  match h with 
  | [] -> failwith "remove_min on empty heap"
  | e :: hh -> (e, hh)
;;

