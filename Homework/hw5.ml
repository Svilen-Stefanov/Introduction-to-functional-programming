(*Aufgabe 5.5*)
let rec superfib n = if n <= 2 then n else superfib (n-1) +  superfib (n-2) + superfib (n-3)    (*works*)

let inv_cantor z =   (*works*)
        let j = int_of_float(sqrt(0.25 +. (float_of_int)(2 * z)) -. 0.5)
        and i = int_of_float(sqrt(0.25 +. (float_of_int)(2 * z)) -. 0.5) in
        ((j - (z - j*(j+1)/2)) , (z - i*(i+1)/2))

(* Test for inv_cantor*)
(*
  let (x,y) = inv_cantor 12;;
  print_int (fst (x,y));
  print_newline();
  print_int (snd (x,y));
  print_newline();
*)

(*Aufgabe 5.6*)

let rec scale_apply list1 list2 = match list1 
    with a::b::c::tl -> (match list2 with
      x :: xs ->
        (a * (x b) + c)::scale_apply tl xs
        | _ -> [])
    | [a] -> []
    | [a;b] -> []
    | _ -> []
(*list1 = Liste mit functionen
  list2 = Liste mit integers
  getestet mit dem Beispiel aus Blatt 5*)

let rec is_insert funct i = function 
    x::xs ->          
      if(funct (i:int) (x:int)) 
        then i::x::xs
        else x::is_insert funct i xs
    | _ -> i::[]
(*insert at the right place*)

let rec insertion_sort list1 funct = match list1 with 
    x::xs ->  is_insert funct x (insertion_sort xs funct)
    | _ -> []   
(*insertion sort*)

(*Aufgabe 5.7*)
type person = {
  name : string;
  age : int;
}
[@@deriving show]

type tree =
  Node of tree * tree * person
  | Leaf
[@@deriving show]

let singleton p = Node(Leaf, Leaf, p)

let rec insert person = function 
    Node (left, right ,p) -> 
      if(p.name >= person.name)    (*works*)
        then Node (insert person left, right, p) 
        else Node (left, insert person right, p)    
    | _ -> Node (Leaf, Leaf, person)

(*Tested with Example

    tree = Node (Node 
          (Node (Leaf, Leaf, {name = "Aab"; age = 22}), Leaf, {name = "Anna"; age = 11}),
          Leaf, {name = "Ivan"; age = 15})

*)

let rec to_sorted_list = function
    Node (left, right ,person) ->
      to_sorted_list left @ person :: to_sorted_list right
    | _ -> [];;

(*result:
[{name = "Aab"; age = 22}; {name = "Anna"; age = 11};
 {name = "Ivan"; age = 15}]

*)
(*print_endline ([%derive.show: tree] t);*)
