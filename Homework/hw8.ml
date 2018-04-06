open Hw8_task

(* 8.3 - 1 *)
let rec get_edges_in (edges : edge list) (n : node_id) acc = 
    match edges with 
        x::xs -> (match x with ((node1:node_id), _ , _ , (node2 : node_id)) -> 
                    if(node2 = n) then get_edges_in xs n (acc@x::[])
                    else get_edges_in xs n acc
                )       
        |_ -> acc

let get_in_edges (c : cfg) (n : node_id) : edge list = 
    get_edges_in c.edges n []

let rec get_edges_out (edges : edge list) (n : node_id) acc = 
    match edges with 
        x::xs -> (match x with ((node1:node_id), _ , _ , (node2 : node_id)) -> 
                    if(node1 = n) then get_edges_out xs n (acc@x::[])
                    else get_edges_out xs n acc
                )       
        |_ -> acc

let get_out_edges (c : cfg) (n : node_id) : edge list = get_edges_out c.edges n []

(* 8.3 - 2 *)
let rec must_compute_wp_rec (edges : edge list) = 
    match edges with x::xs -> 
        (match x with ( _ , _ , None , _ ) -> true
                      |_ -> must_compute_wp_rec xs
        )
    |_-> false

let must_compute_wp (c : cfg) (n : node_id) : bool = 
    must_compute_wp_rec (get_in_edges c n)

(* 8.3 - 3 *)
let rec can_compute_wp_rec (edges : edge list) = 
     match edges with x::xs -> 
        (match x with ( _ , _ , (Some f : formula option) , _ ) -> can_compute_wp_rec xs
                      |_ -> false 
        )
    |_-> true

let can_compute_wp (c : cfg) (n : node_id) : bool = 
    can_compute_wp_rec (get_out_edges c n)

(* 8.3 - 4 *)
let rec set_wp_rec (c : cfg) (edges : edge list) (n : node_id) (f : formula) (acc : edge list) =
    match edges with 
        x::xs -> (match x with ((node1:node_id), (guard : guard option)  , _ , (node2 : node_id)) -> 
                    if(node2 = n) then 
                    set_wp_rec c xs n f (acc@(node1, guard, Some f , node2)::[])
                    else set_wp_rec c xs n f (acc@x::[])
                )       
        |_ -> {nodes = c.nodes; edges = acc}

let set_wp (c : cfg) (n : node_id) (f : formula) : cfg = 
    set_wp_rec c c.edges n f []

(* 8.3 - 5 *)
let is_constant (exp : linear_expression) = 
    match exp with 
    LinExpr(int1,var,int2) -> if(int1 = 0) then Constant (int2)
                              else exp
    |_-> exp

let is_constant_le (exp : linear_expression) = 
    match exp with 
    LinExpr(int1,var,int2) -> if(int1 = 0) then Constant (int2 - 1)
                              else LinExpr(int1,var,int2 - 1)
    |Constant(n)-> Constant(n - 1)

let normalize_boolean_expression (bexpr : boolean_expression) : boolean_expression = 
    match bexpr with True -> True
                     |False -> False
                     |BoolExpr(exp1, bool_exp, exp2) -> 
                        match bool_exp with 
                        |Le -> BoolExpr(is_constant exp1, Leq, is_constant_le exp2)  
                        |_-> BoolExpr(is_constant exp1, bool_exp, is_constant exp2) 
                                            

(* 8.3 - 6 *)
let negate_boolean_expression (bexpr : boolean_expression) : boolean_expression = 
    match bexpr with True -> False
                     |False -> True
                     |BoolExpr(exp1, bool_exp, exp2) -> 
                        match bool_exp with 
                         Eq -> BoolExpr(exp1, Neq, exp2) 
                         |Neq -> BoolExpr(exp1, Eq, exp2) 
                         |Le -> BoolExpr(exp2, Leq, exp1) 
                         |Leq -> BoolExpr(exp2, Le, exp1)                   

(* 8.3 - 7 *)
let check_constants c1 (exp : boolean_expression_op) c2 = 
    match exp with
    Eq -> if(c1 = c2) then True
          else False
    |Neq -> if(c1 <> c2) then True
            else False
    |Le -> if(c1 < c2) then True
           else False
    |Leq -> if(c1 <= c2) then True
            else False

let check_expressions exp1 bool_exp exp2 = 
    match exp1 with
       Constant c1 -> (
          match exp2 with 
          Constant c2-> check_constants c1 bool_exp c2 
          |LinExpr(int1,var,int2) -> False
       )
       |LinExpr(int1,var,int2) -> False

let rec simplify_boolean_expression (bexpr : boolean_expression) : boolean_expression = 
    match bexpr with True -> False
                     |False -> True
                     |BoolExpr(exp1, bool_exp, exp2) -> check_expressions exp1 bool_exp exp2        

(* 8.3 - 8 *)
let simplify_implication (impl : implication) : implication = 
    match impl.conclusion with 
        False -> { assumption = True ; conclusion = negate_boolean_expression impl.assumption}
        |_ -> { assumption = simplify_boolean_expression impl.assumption ; conclusion = simplify_boolean_expression impl.conclusion}

(* 8.3 - 9 *)
let isLeq (exp1 : linear_expression) (exp2 : linear_expression) = 
    match exp1 with
    Constant c1 -> (
        match exp2 with 
            Constant c2 -> if (c1 <= c2) then true else false
            |_ -> false )
    |LinExpr(int1,var,int2) -> (
        match exp2 with 
            LinExpr(int3,var2,int4) -> if(int1 = int3 && var = var2 && int2 = int4) then true else false 
            |_ -> false )

let isLe (exp1 : linear_expression) (exp2 : linear_expression) = 
    match exp1 with
    Constant c1 -> (
        match exp2 with 
            Constant c2 -> if (c1 < c2) then true else false
            |_ -> false )
    |LinExpr(int1,var,int2) -> (
        match exp2 with 
            LinExpr(int3,var2,int4) -> if(int1 = int3 && var = var2 && int2 < int4) then true else false 
            |_ -> false )

let is_tautology (impl : implication) : bool = 
    match impl.assumption with 
        False -> true
        |BoolExpr(exp1, bool_exp1, exp2) -> (
                        match impl.conclusion with
                            True -> true
                            |BoolExpr(exp3, bool_exp2, exp4) ->
                                if(exp1 = exp3 && bool_exp1 = bool_exp2 && exp2 = exp4) then true
                                else if(bool_exp1 = bool_exp2 && (bool_exp1 = Eq || bool_exp1 = Neq)) then true
                                else if(bool_exp1 = bool_exp2 && bool_exp1 = Leq && 
                                    ((exp1 = exp3 && isLeq exp2 exp4) || (exp2 = exp4 && isLeq exp3 exp1))) then true
                                else if(bool_exp1 = bool_exp2 && bool_exp1 = Le && 
                                    ((exp1 = exp3 && isLe exp2 exp4) || (exp2 = exp4 && isLe exp3 exp1))) then true else true
                            |_ -> false
                        )
        |True -> match impl.conclusion with
                            True -> true
                            |_ -> false 
                        

(* 8.3 - 10 *)
let rec check_for_tautologies acc (f : formula) : formula = 
    match f with
    x::xs -> if(is_tautology x) then check_for_tautologies acc xs 
             else (acc@x::[])
    |_ -> acc

let rec simplify_formula_rec (f : formula) acc : formula= 
    match f with
    x::xs -> (acc@(simplify_implication x)::[])
    |_ -> acc

let simplify_formula (f : formula) : formula = 
    simplify_formula_rec f [] |> check_for_tautologies []

(* 8.3 - 11 *)
let replace_var_in_linear_expression (expr : linear_expression) (v : var) 
    (e : linear_expression) : linear_expression = 
     match expr with
     Constant c1 -> Constant(c1)     
     |LinExpr(int1,var,int2) -> (
        match e with 
            LinExpr(int3,var2,int4) -> LinExpr(int1*int3,v, int1*int4 + int2)
            |Constant c -> Constant(int1*c + int2) )

let replace_var_in_boolean_expression (bexpr : boolean_expression) (v : var) 
    (e : linear_expression) : boolean_expression = 
     match bexpr with
     BoolExpr(exp1, bool_exp, exp2) -> BoolExpr(replace_var_in_linear_expression exp1 v e, bool_exp, replace_var_in_linear_expression exp2 v e)
     |_ -> bexpr

(* 8.3 - 12 *)
let compute_wp (c : cfg) (n : node_id) : formula = failwith "TODO"

(* 8.3 - 13 *)
let rec verify (c : cfg) : cfg = 
    let rec verify_rec nodes = 
        match nodes with 
        x::xs -> (match x with 
                (node_id, node) -> 
                    if (must_compute_wp c node_id && can_compute_wp c node_id) then verify (set_wp c node_id (compute_wp c node_id))
                    else verify_rec xs
                 )
        |_ -> c
    in verify_rec c.nodes
