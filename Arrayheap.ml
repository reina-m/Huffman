type tree = 
  | Leaf of char 
  | Node of tree * tree 
;;

type t = (int * tree) array

let empty = [||]

let is_singleton h = Array.length h = 1

let is_empty h = Array.length h = 0

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let rec remonte_heap h i =
  if i > 0 then 
    let parent_index = (i - 1) / 2 in (* formule pour avoir l'indice du parent situe a l'indice i *)
    if fst h.(i) < fst h.(parent_index) then (
      swap h i parent_index;
      remonte_heap h parent_index
    )


let rec add x h = 
  let h' = Array.append h [| x |] in
  remonte_heap h' (Array.length h' - 1);
  h'

let find_min h = 
  if is_empty h then failwith "find_min on empty heap"
  else h.(0)

let rec descend_heap h i =
  let sag = 2 * i + 1 in  
  let sad = 2 * i + 2 in  
  let len = Array.length h in
  let min_index =
    if sag < len && fst h.(sag) < fst h.(i) then sag else i
  in
  let min_index =
    if sad < len && fst h.(sad) < fst h.(min_index) then sad else min_index
  in

  if min_index <> i then (
    swap h i min_index;
    descend_heap h min_index
  )

let remove_min h = 
  if is_empty h then failwith "remove_min on empty heap"
  else if is_singleton h then (h.(0), [||])
  else
    let last = h.(Array.length h - 1) in
    let h' = Array.copy h in
    h'.(0) <- last;
    let h'' = Array.sub h' 0 (Array.length h - 1) in
    descend_heap h'' 0;
    (h.(0), h'')
  
    
  (*if is_empty h then failwith "remove_min on empty heap"
  else if is_singleton h then (h.(0), [||])
  else 
    let dernier_elem = h.(Array.length h - 1) in
    let h1= Array.copy h in 
    h1.(0) <- dernier_elem;
    let h2 = Array.make (Array.length h - 1) h1.(0) in
    for i = 0 to Array.length h - 2 do
      h2.(i) <- h1.(i + 1)
    done;
    descend_heap h2 0;
    (h1.(0), h2) *)
