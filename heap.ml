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
  remonte_heap h' (List.length h)

let find_min h = 
  match h with 
  | [] -> failwith "find_min on empty heap"
  | e :: _ -> e
;;

let rec descend_heap h i =
  let sag = 2 * i + 1 in  
  let sad = 2 * i + 2 in  
  let len = List.length h in

  let min_index =
    if sag < len && fst (List.nth h sag) < fst (List.nth h i) then sag else i
  in
  let min_index =
    if sad < len && fst (List.nth h sad) < fst (List.nth h min_index) then sad else min_index
  in

  if min_index <> i then
    let tmp = List.nth h i in
    let h' = List.mapi (fun idx elem ->
      if idx = i then List.nth h min_index
      else if idx = min_index then tmp
      else elem
    ) h in
    descend_heap h' min_index
  else h


let remove_min h = 
  match h with 
  | [] -> failwith "remove_min on empty heap"
  | [e] -> (e, [])  
  | e :: hh -> 
    let dernier_elem = List.nth h (List.length h - 1) in
    let h1 = dernier_elem :: (List.tl hh) in
    (e, descend_heap h1 0)
