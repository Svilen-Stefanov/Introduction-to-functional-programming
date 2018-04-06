type bin_tree =
    Node of bin_tree_node
  | Leaf
and bin_tree_node = { 
  key : int;
  left : bin_tree;
  right : bin_tree;
}
[@@deriving show]

let rec insert key = function
    Node n ->
    if key <= n.key then Node {n with left = insert key n.left}
    else Node {n with right = insert key n.right}
  | Leaf -> Node { key; left = Leaf; right = Leaf } 

type variable = { name : string }
and binding = { is_rec : bool; var : variable; expr : expression; inner : expression }
and func = { param : variable; body : expression }
and closure = { param : variable; body : expression }
and funcApp = { f : expression; param_act : expression }
and ifThen = { cond: expression; then_branch: expression; else_branch: expression}
and immediate =
    Bool of bool
  | Integer of int
and expression =
    Variable of variable
  | Immediate of immediate
  | BinOperation of binop
  | UnOperation of unop
  | Binding of binding
  | Func of func
  | Closure of closure 
  | FuncApp of funcApp
  | IfThenElse of ifThen
and binop_op =
    Addition
  | Subtraction
  | Multiplication
  | CompareEq
  | CompareLess
  | And
  | Or
and binop = binop_op * expression * expression
and unop_op =
    Negate
  | Not
and unop = unop_op * expression
and program = expression
[@@deriving show]

let add a b = BinOperation(Addition, a, b)
let sub a b = BinOperation(Subtraction, a, b)
let mul a b = BinOperation(Multiplication, a, b)
let less a b = BinOperation(CompareLess, a, b)
let _eq a b = BinOperation(CompareEq, a, b)
let var v = Variable ({name=v})
let imm i = Immediate (Integer i)
let _let v e inner = Binding {is_rec = false; var={name=v}; expr=e; inner=inner}
let _let_rec v e inner = Binding {is_rec = true; var={name=v}; expr=e; inner=inner}
let _fun p b = Func { param = {name=p}; body = b}
let _fun_xx p q b = _fun p (_fun q b)
let apply f p = FuncApp {f = f; param_act = p}
let apply_xx f p q = apply (apply f p) q
let ite c t e = IfThenElse { cond = c; then_branch = t; else_branch = e}