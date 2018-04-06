(*Aufgabe 1*)

type bin_tree =
Node of bin_tree_node
| Leaf
and bin_tree_node = {
key : int;
left : bin_tree;
right : bin_tree;
}
[@@deriving show]

let rec insert i tree = math tree with
    Node {key = k; left = l; right = r} ->
      if i>k
        then Node {key = k; left = l; right = insert i r}
        else Node {key = k; left = insert i l; right = r)}
  | _ -> Node { key = i; left = Leaf; right = Leaf }

(*verbesserte Variante:*)

let rec insert i = funktion
    Node {key; left; right} ->
      if i>k
        then Node {key; left; right = insert i r}
        else Node {key; right; left = insert i l)}
  | _ -> Node { key = i; left = Leaf; right = Leaf }

(*oder noch besser*)

let rec insert i = function
    Node n ->
      if i>n.key
        then Node {n with right = insert i n.right}
        else Node {n with left = insert i n.left}
  | _ -> Node { key = i; left = Leaf; right = Leaf }


let tree = Node {
      key = 2;
      left = Node{key = 1; left = Leaf; right = Leaf};
      right = Node{key = 4; left = Leaf; right = Leaf}
  };;
insert 0 tree

let rec sum = function
    Leaf n -> n.key
  | _ -> n.key + sum n.left  + sum n.right

let rec height =
