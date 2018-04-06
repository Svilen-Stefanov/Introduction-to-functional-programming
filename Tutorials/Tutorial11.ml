(*11.2*)
module MakeExprEvaluator (O : Ops) :
  ExprEvaluator with type elem := O.elem = struct
  type elem = O.elem
  type t = elem expr

  let rec evaluate = function
    Const c -> c
    | Addition (a, b) -> O.add (evaluate a) (evaluate b)
    | Subtraction (a, b) -> O.sub (evaluate a) (evaluate b)
    | Multiplication (a, b) -> O.mul (evaluate a) (evaluate b)
end

module IntExprEvaluator : ExprEvaluator with type elem := int
= MakeExprEvaluator (struct
    type elem = int

    let add = ( + )
    let sub = ( - )
    let mul = ( * )
end)

module FloatExprEvaluator = MakeExprEvaluator (struct
    type elem = int

    let add = ( +. )
    let sub = ( -. )
    let mul = ( *. )
end)

module VectorExprEvaluator ( = MakeExprEvaluator (struct
    type elem = int list

    let op f a b = List.map2 f a b
    let add = op ( + )
    let sub = op ( - )
    let mul = op ( * )
end)

(*allgemeiner*)
module VectorExprEvaluator (O : Ops) = MakeExprEvaluator (struct
    type elem = O.elem list

    let op f a b = List.map2 f a b
    let add = op O.add
    let sub = op O.sub
    let mul = op O.mul
end)

(*11.3*)

module Boolean : Number = struct 
    type z = bool
    let zero = false
    let ( +. ) = ( || )
    let ( *. ) = ( && ) 
    let string_of_number (z : z) = match z with
                            true -> "1"
                            |_ -> "0"

module MinPlusNat : Number = struct 
    type z = bool
    let zero = false
    let ( +. ) = ( || )
    let ( *. ) = ( && ) 
    let string_of_number (z : z) = match z with
                            true -> "1"
                            |_ -> "0"
