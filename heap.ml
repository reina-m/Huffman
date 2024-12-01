type 'a t = 'a list (* remplacer par une définition qui vous convient *)

let empty = []

let is_singleton t = 
  match t with 
  | [_] -> true 
  | _ -> false 
;;

let is_empty t = 
  t = empty (*à tester*)
;;

let rec add t x = 
  match t with 
  | [] -> [x]
  | e :: tt ->  
    if x <= e then x :: e :: tt
    else e :: add tt x
;;

let find_min t = 
  match t with 
  | [] -> failwith "find_min on empty heap"
  | e :: _ -> e (*car c'est un tas?*)
;;

let remove_min t = 
  match t with 
  | [] -> failwith "remove_min on empty heap"
  | e :: tt -> (e, tt)
;;
