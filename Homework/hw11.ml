open Hw11_task

(*11.4*)
module MakeHashMap (X:Hashable) : Map  with type key = X.key = struct
  type key = X.key

  type 'v t = { mutable elem_count : int;
                buckets: (key * 'v) list array; 
                hash_func: key -> int; 
              }

  let create () : 'v t = {elem_count = 0;
                          buckets = Array.make 37 [];
                          hash_func = X.hash;
                          }

  let size (t : 'v t) = t.elem_count

  let bucket_has_key t i key = 
  List.exists (fun (key',_) -> key' = key) t.buckets.(i) 

  let bucket_has_key_option t i key = 
  if(List.exists (fun (key',_) -> key' = key) t.buckets.(i)) then 
    let pair = List.find (fun (key',_) -> key' = key) t.buckets.(i) in Some (snd pair)
  else None 

  let insert (t : 'v t) (key : key) data = 
    let i = (t.hash_func key) mod 37 in
    let has_key = bucket_has_key t i key in
    let filter_bucket =
      if has_key then
        List.filter (fun (key',_) -> key' <> key) t.buckets.(i) 
      else
        t.buckets.(i)
    in
    t.buckets.(i) <- (key, data) :: filter_bucket;
    if not has_key then t.elem_count <- t.elem_count + 1;
    t

  let remove (t : 'v t) (key : key) = 
    let i = (t.hash_func key) mod 37 in
    let has_key = bucket_has_key t i key in
    if has_key then (
      let filter_bucket =
        List.filter (fun (key',_) -> key' <> key) t.buckets.(i) 
      in
      t.buckets.(i) <- filter_bucket;
      t.elem_count <- t.elem_count - 1;
      t
    ) else t

  let lookup (t : 'v t) (key : key) = 
    let i = (t.hash_func key) mod 37 in
    bucket_has_key_option t i key
end


module MakeTreeMap (X : Comparable) : Map with type key = X.key = struct
  type key = X.key

  type 'v t = { mutable elem_count : int;
                mutable tree: (key * 'v) bin_tree; 
                cmp_func: key -> key -> int; 
              }

  let create () = {elem_count = 0;
                tree = Leaf;
                cmp_func = X.compare;
                }

  let size (t : 'v t) = t.elem_count

  let rec tree_has_key t x compare = match t with
    Node n ->
    let comp = compare x n.data in
    if comp = 0 then true
    else if comp < 0 then tree_has_key n.left x compare
    else tree_has_key n.right x compare
  | Leaf -> false  

  let rec tree_has_key_optional t x compare = match t with
    Node n ->
    let comp = compare x n.data in
    if comp = 0 then Some (snd n.data)
    else if comp < 0 then tree_has_key_optional n.left x compare
    else tree_has_key_optional n.right x compare
  | Leaf -> None 

  let compare_key compare x y = compare (fst x) (fst y)

  let insert (t : 'v t) (key : key) data = 
    let has_key = tree_has_key t.tree (key,data) (compare_key X.compare) in
    if has_key then (
      let new_tree = remove (t.tree) (key,data) (compare_key X.compare) in 
      t.tree <- insert new_tree (key,data) (compare_key X.compare);
      t
    )
    else (
      t.elem_count <- t.elem_count + 1;
      t.tree <- insert (t.tree) (key,data) (compare_key X.compare);
      t
    ) 

  let remove (t : 'v t) (key : key) = 
    let has_key = tree_has_key t.tree (key,key) (compare_key X.compare) in 
    if has_key then (
      t.elem_count <- t.elem_count - 1;
      t.tree <- remove (t.tree) key (fun key (key', _) -> X.compare key key');
      t
    ) else t 

  let lookup (t : 'v t) (key : key) = tree_has_key_optional t.tree key (fun key (key', _) -> X.compare key key')
end


module IntHashMap = MakeHashMap (struct (*Hashable*)
  type key = int
  let hash key = Hashtbl.hash key
end);;


module IntTreeMap = MakeTreeMap (struct (*Comparable*)
  type key = int
  let compare (x : key) (y : key) = if x > y then 1 else (if x = y then 0 else -1)
end);;

(*11.5*)
module Lift (X:Base) : Extended with type 'a t = 'a X.t = struct
  include X
  let iter f l = X.fold (fun l x -> f l) l ()
  let map f l = X.fold (fun x a -> X.insert (f x) a) l X.empty
  let filter f l = X.fold (fun x a -> if f x then X.insert x a else a) l X.empty
  let append l1 l2 = X.fold (fun x a -> X.insert x a) l1 l2 
  let flatten (l : 'a t t) : 'a t = X.fold (fun x xs -> append x xs) l X.empty
  let to_list (l : 'a t) : 'a list = X.fold (fun l b -> l::b) l [] 
  let of_list (l : 'a list) :  'a t = List.fold_right (fun a b -> insert a b) l X.empty
end

module List : Base = struct 
  type 'a t = 'a list
  let empty : 'a t = [] 
  let insert x l =  List.sort (fun a b -> if a > b then 1 else (if a = b then 0 else -1)) (x::l)
  let fold f l b = List.fold_right f l b
end

module SearchTree : Base = struct
  type 'a t = 'a bin_tree
  let empty : 'a t = Leaf
  let insert x tree  = insert tree x (fun a b -> if a > b then 1 else (if a = b then 0 else -1))
  let rec fold (f : ('a -> 'b -> 'b)) (a : 'a t) (b : 'b) = match a with 
      Node{data = x; left = l; right = r} -> f x (fold f l b); f x (fold f r b)
      |Leaf -> b
end

module ExtendedList = Lift (List)

module ExtendedSearchTree = Lift (SearchTree)
