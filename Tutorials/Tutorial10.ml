(*10.1*)
(*
Wozu dienen Module? 
    Komplexität zu kapseln - Sachen zusammenpacken, die iwie zusammen gehören

Wieso definiert man Module und Signaturen oft getrennt?
    Abstrahierung, damit man nicht genau wissen muss, wie der Modul implementiert ist. 
    Deshalb steht auch die Signatur da. Wenn keine Signatur für einen Modul gibt, dann ist auch der Modul nicht von außen sichtbar.

Wieso kann es sinnvoll sein, Typen in Signaturen abstrakt zu lassen und welche Konsequenzen hat es?
    Sicherheit / Information hiding = public/private
Welche Parallelen zwischen Modulen und bekannten Konzepten, insbesondere in Java, gibt es?
    Signatur = Interface
    Information hiding = public/private
*)

(*10.2*)
module type Heap = sig
type 'a t
exception Empty
val create : unit -> 'a t
val is_empty : 'a t -> bool
val insert : 'a -> 'a t -> 'a t
val delete_max : 'a t -> ('a * 'a t)
end

type 'a btree =
Leaf
| Node of int * 'a * 'a btree * 'a btree

module BinTreeHeap : Heap = struct
type 'a t

exception Empty

let create () = Leaf

let is_empty = function
    Leaf -> true
    |_ -> false


let rec insert x =  failwith "TODO"

let delete_max = function
      Leaf -> raise Empty
    | Node(_, x, t1, t2) -> x, merge (t1 t2)
and merge = function
    |Leaf, t | t, Leaf -> t
    |t1,t2 -> merge (let v, r = delete_max t2 in insert v t1, r)
end

module BinTreeHeap : Heap = struct
  type 'a btree = Leaf | Node of int * 'a * 'a btree * 'a btree 
  type 'a t = 'a btree
  exception Empty

  let create () = Leaf

  let is_empty = function
      Leaf -> true
    | _ -> false

  let min_way = function
      Leaf -> 0
    | Node (c, _, _, _) -> c

  let rec insert x = function
      Leaf -> Node (1, x, Leaf, Leaf)
    | Node(_,x',t1,t2) -> 
      let (s,ns) = if x > x' then (x', x) else (x, x') in
      let (t1,t2) = if min_way t1 <= min_way t2 then
          (insert s t1, t2) 
        else (t1, insert s t2) in
      Node(1 + min (min_way t1) (min_way t2), ns, t1, t2)


  let rec repair t1 t2 =
    match (t1, t2) with
      (Leaf, t) | (t, Leaf) -> t
    | (Node (_, x1, t1', t1''), Node (_, x2, t2', t2'')) -> 
      let (x, t1, t2) = if x1 > x2 then 
          (x1, repair t1' t1'', t2) 
        else (x2, t1, repair t2' t2'') in
      Node(1 + min (min_way t1) (min_way t2), x, t1, t2)

  let delete_max = function
      Leaf -> raise Empty
    | Node(_, x, t1, t2) -> (x, repair t1 t2)
end

(*
let delete_max = function
      Leaf -> raise Empty
    | Node(_, x, t1, t2) -> x, merge (t1 t2)
and merge = function
    |Leaf, t | t, Leaf -> t
    |t1,t2 -> merge (let v, r = delete_max t2 in insert v t1, r)
end
*)

let heapsort l = 
  let rec build_list acc heap =
    if BinTreeHeap.is_empty heap then
      acc else
      let (x,heap) = BinTreeHeap.delete_max heap in
      build_list (x::acc) heap in
  let build_list l = build_list [] l in
  build_list (List.fold_left (fun heap x ->
      BinTreeHeap.insert x heap) (BinTreeHeap.create ()) l) 

let factors n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then



