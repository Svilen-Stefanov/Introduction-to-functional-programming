open Hw7_task

let rec sum l acc = match l with
    x::xs -> sum xs (acc+x)
    |_-> acc

let rec hyperfib k n i l = if k = 1 then 1
        else if n<= k then n  (*l - list*)
        else if i>k then sum l 0 (*summe*)
        else hyperfib k n (i+1) ((hyperfib k (n-1) (i+1) [])::l)

let hyperfib k n = hyperfib k n 0 [] 

let rec count n i = if n = 1 then i + 1
            else if n mod 2 = 0 then count (n/2) (i+1)
            else count ((3*n)+1) (i+1)

let coco n = count n 0 

let rec sortedList tree l = match tree with 
    Node (n) -> sortedList (n.left) (n.key::(sortedList n.right l))
    | _ -> l

let to_sorted_list tree = match tree with 
    Node (n) -> sortedList tree []
    | _ -> []

let testImmediate var = match var with 
    Integer i -> i 
    |_ -> failwith ""

let castToBool var = match var with
    Bool b -> b
    |_ -> failwith ""

let handleVar v params = Integer (List.nth params (int_of_string (String.sub v 1 ((String.length v) - 1))))

let funct a b = (a,b)

let handleFun (a,b) = b

let rec evaluate (params:int list) (prog:program) = match prog with (**)
    Immediate(n) -> n
    |Variable(v) -> handleVar v.name params
    |BinOperation(Addition, a, b) -> Integer(testImmediate (evaluate params a) + testImmediate (evaluate params b))
    |BinOperation(Subtraction, a, b) -> Integer(testImmediate (evaluate params a) - testImmediate (evaluate params b))
    |BinOperation(Multiplication, a, b) -> Integer(testImmediate (evaluate params a) * testImmediate (evaluate params b))
    |BinOperation(CompareLess, a, b) -> Bool(testImmediate (evaluate params a) < testImmediate (evaluate params b))
    |BinOperation(CompareEq, a, b) -> Bool(testImmediate (evaluate params a) = testImmediate (evaluate params b))
    |BinOperation(And, a, b) -> Bool(castToBool (evaluate params a) && castToBool (evaluate params b))
    |BinOperation(Or, a, b) -> Bool(castToBool (evaluate params a) || castToBool (evaluate params b))
    |UnOperation(Negate, a) -> Integer((-(testImmediate (evaluate params a))))
    |UnOperation(Not, a) -> Bool(not(castToBool (evaluate params a)))
    |Func{param; body} -> evaluate params (handleFun (funct param body)) 
    |IfThenElse{cond; then_branch; else_branch} -> 
        if(castToBool(evaluate params cond))
            then evaluate params then_branch 
        else evaluate params else_branch
    (*|Binding{is_rec;var;expr;inner} -> match is_rec with 
        false -> (fun (evaluate params var) -> (evaluate params expr)) evaluate params inner
        |true ->  (*rec*)
    |Closure{param; body} ->  
    |FuncApp{f;param_act} ->*)
    |_->failwith ""

let evaluate params prog = evaluate params prog;;


