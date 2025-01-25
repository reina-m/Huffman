type tree = 
  | Leaf of char 
  | Node of tree * tree 
;;

type t = (int * tree) array
(** The type of heaps. Elements are ordered using generic comparison.
*)

val empty : t
(** [empty] is the empty heap. *)

val add : (int * tree) -> t -> t
(** [add e h] add element [e] to [h]. *)

val find_min : t -> (int * tree)
(** [find_min h] returns the smallest elements of [h] w.r.t to 
    the generic comparison [<] *)

val remove_min : t -> (int * tree) * t
(** [remove_min h] returns the pair of the smallest elements of [h] w.r.t to 
    the generic comparison [<] and [h] where that element has been removed. *)

val is_singleton : t -> bool
(** [is_singleton h] returns [true] if [h] contains one element *)

val is_empty : t -> bool
(** [is_empty h] returns [true] if [h] contains zero element *)