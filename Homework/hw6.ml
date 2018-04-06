open Hw6_task

let rec height = function
    Node(n) ->
        1 + (max (height (n.left)) (height (n.right)))
    | Leaf -> 0

let rec to_sorted_list = function
    Node(n) ->
      to_sorted_list n.left @ n.key :: to_sorted_list n.right
    | _ -> [];;

let rec isSorted = function
     x::y::xs ->
        if(x <= y) then isSorted (y::xs)
        else false
    | _ -> true

let rec valid_avl t = match t with
    Node(n) ->
        let hl = height n.left and hr = height n.right in
        if (n.balance >= -1 && n.balance <= 1 && hr - hl == n.balance
            && isSorted (to_sorted_list t) && valid_avl n.left && valid_avl n.right)
            then true
        else false
    | _ -> true

let rec print t = match t with
    Node (n) -> (
            match n.left with
            Node (x) -> (
                match n.right with
                Node (y) -> (*when left and rights nodes*)
                    "\n" ^ string_of_int(n.key) ^ " -- " ^ string_of_int(x.key) ^ "\n" ^ string_of_int(n.key) ^ " -- " ^ string_of_int(y.key)
                    ^ print n.left ^ print n.right
                | _ -> "\n" ^ string_of_int (n.key) ^ " -- " ^ string_of_int(x.key) ^ print n.left
            )
            | _ -> (    (*when left Leaf is*)
                match n.right with (*when only right Node is*)
                Node (y) -> "\n" ^ string_of_int (n.key) ^ " -- " ^ string_of_int(y.key) ^ print n.right
                | _ -> "" (*when both Leafs*)
            )
        )
    | _ -> ""

let dot t = "graph avl {" ^ print t ^ "\n}"

let rotate_left t = match t with
    Node(n) -> (
        match n.right with
        Node (x) ->
          Node{key = x.key; balance = 0; left = Node{key = n.key; balance = 0; left =  n.left; right = x.left}; right = x.right}
        | _ -> t)
    | _ -> t

let rotate_right t =  match t with
    Node(n) -> (
        match n.left with
         Node (x) ->
           Node{key = x.key; balance = 0; left = x.left; right = Node{key = n.key; balance = 0; left = x.right; right = n.right}}
        | _ -> t)
    | _ -> t

let rotate_single dir t =  match dir with
    Left -> rotate_left t
    | _ -> rotate_right t

let rotate_double_left n t = match n.right with
    Node (x) -> (
        match x.left with
        Node (y) -> let a = height y.left and b = height y.right in
                if(a-b = 0) then
                    Node{key = y.key; balance = 0;
                    left = Node{key = n.key; balance = 0; left = n.left; right = y.left};
                    right = Node{key = x.key; balance = 0; left = y.right; right = x.right}
                    }
                else if (a-b > 0) then
                    Node{key = y.key; balance = 0;
                    left = Node{key = n.key; balance = 0; left = n.left; right = y.left};
                    right = Node{key = x.key; balance = 1; left = y.right; right = x.right}
                    }
                else
                    Node{key = y.key; balance = 0;
                    left = Node{key = n.key; balance = -1; left = n.left; right = y.left};
                    right = Node{key = x.key; balance = 0; left = y.right; right = x.right}
                    }
        |_ -> t
    )
    | _ -> t

let rotate_double_right n t = match n.left with
    Node (x) -> (
        match x.right with
        Node (y) -> let a = height y.left and b = height y.right in
                if(a-b = 0) then
                    Node{key = y.key; balance = 0;
                    left = Node{key = x.key; balance = 0; left = x.left; right = y.left};
                    right = Node{key = n.key; balance = 0; left = y.right; right = n.right} (*1*)
                    }
                else if (a-b > 0) then
                    Node{key = y.key; balance = 0;
                    left = Node{key = x.key; balance = 0; left = x.left; right = y.left};
                    right = Node{key = n.key; balance = 1; left = y.right; right = n.right} (*1*)
                    }
                else
                    Node{key = y.key; balance = 0;
                    left = Node{key = x.key; balance = -1; left = x.left; right = y.left};      (*-1*)
                    right = Node{key = n.key; balance = 0; left = y.right; right = n.right}
                    }
        |_ -> t
    )
    | _ -> t

let rotate_double dir t = match dir with
    Left -> (match t with
            Node (n) -> rotate_double_left n t
            |_ -> t)
    | _ -> (match t with
            Node (n) -> rotate_double_right n t
            |_ -> t)

let rebalance t = match t with
    Node(n) -> (
        if (n.balance = 2) then
            match n.right with
            Node x ->
                if (x.balance = 1) then rotate_single Left t
                else if (x.balance = -1) then rotate_double Left t
                else t
            | _ -> t    (*the tree isn't correct*)
        else if (n.balance = -2) then
           match n.left with
            Node x ->
                if (x.balance = -1) then rotate_single Right t
                else if (x.balance = 1) then rotate_double Right t
                else t
            | _ -> t (*not correct*)
        else t
    )
    | _ -> t

(*insert the element at the right place recursively*)
let rec insert_helper x t = match t with
    Node (n) ->
        if(n.key > x) then
            if(n.left <> Leaf) then
                Node{key = n.key; left = insert_helper x n.left; balance = n.balance; right = n.right}
            else Node{key = n.key; balance = n.balance - 1; left = Node{key=x;balance=0;left=Leaf;right=Leaf}; right = n.right}
        else
            if(n.right <> Leaf) then
                Node{key = n.key; right = insert_helper x n.right; balance = n.balance; left = n.left}
            else Node{key = n.key; balance = n.balance + 1; left = n.left; right = Node{key=x;balance=0;left=Leaf;right=Leaf}}
    | _ -> Node{key=x;balance=0;left=Leaf;right=Leaf}

(*recalculates the balance of the tree*)
let rec recalculate t = match t with
    Node(n) ->
        rebalance (Node{key = n.key; balance = ((height n.right) - (height n.left)); left = recalculate (n.left); right = recalculate (n.right)})
    | _ -> t

let insert x t = match t with
    Node(n) -> rebalance (recalculate (insert_helper x t))
    | _ -> Node{key=x;balance=0;left=Leaf;right=Leaf}
