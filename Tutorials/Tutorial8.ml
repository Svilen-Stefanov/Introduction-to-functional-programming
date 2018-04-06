(*Aufgabe 1*)

(*1*)
let f1 : formula = [{
  assumption = BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "b", 0));
  conclusion = BoolExpr (LinExpr (1, "a", 0), Eq, Constant 3);
}]

(*2*)
let f2 : formula = [{
  assumption = BoolExpr (Constant 17, Leq, LinExpr (1, "x", 0));
  conclusion = BoolExpr (LinExpr (1, "y", 0), Leq, LinExpr (1, "z", 0));
}]

(*3*)
let f3 : formula = [{
  assumption : BoolExpr (True);
  conclusion : BoolExpr (LinExpr (1, "u", 0), Neq, LinExpr (1, "v", 0));
}]

(*4*)
let f4 : formula = [{
    assumption=True;
    conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "b", 0))
    }; {
    assumption=True;
    conclusion=BoolExpr (LinExpr (1, "c", 0), Le, LinExpr (1, "k", 0))
}]

(*5*)
let f5 : formula = [{
  assumption : BoolExpr (LinExpr (1, "x", 4), Le, Constant 8);
  conclusion : BoolExpr (False);
}]

(*6*)
let f6 : formula = [{
  assumption : BoolExpr (True);
  conclusion : BoolExpr (LinExpr (-5, "i", 8), Neq, LinExpr (1, "i", 0));
}; {
  assumption : BoolExpr (True);
  conclusion : BoolExpr (LinExpr (1, "g", 0), Neq, LinExpr (1, "n", 0));
}]

(*7*)   (*LANG*)
let f7 : formula = [{
  assumption=BoolExpr (LinExpr (1, "u", 0), Leq, LinExpr (9, "h", (-1)));
  conclusion=BoolExpr (Constant 9, Le, Constant 8)
  }; {
  assumption=BoolExpr (Constant 7, Eq, Constant 2);
  conclusion=BoolExpr (LinExpr (1, "u", 0), Leq, Constant 21)
  }; {
  assumption=BoolExpr (LinExpr (1, "t", 0), Eq, LinExpr (1, "r", 0));
  conclusion=BoolExpr (LinExpr (1, "b", 0), Neq, Constant 21)
}]

(*8*)
let f8 : formula = [{
  assumption=False;
  conclusion=BoolExpr (LinExpr (1, "s", 0), Eq, Constant 42)
}]

(*9*)
let f9 : formula = [{
  assumption : BoolExpr (True);
  conclusion : BoolExpr (False);
}]

(*10*)
let f10 : formula = [{
  assumption : BoolExpr (True);
  conclusion : BoolExpr (True);
}]

(*Aufgabe 2*)
(*WICHTIG!!!! - > List.assoc*)

(*1*)
let g1 : cfg = {
  nodes = [
    (0, Start);
    (1, Statement (Read "x"));
    (2, Stop);
  ];
  edges = [
    (0, None, None, 1);
    (1, None, None, 2);
  ];
}

(*3*)
let g3 : cfg = {
  nodes = [
    (0, Start);
    (1, Statement (Assignment ("a", Constant 4)));
    (2, Branch (BoolExpr (LinExpr (1, "a", 0)), Ne, Constant 0));
    (3, Statement (Assignment ("a", LinExpr (-1, "a", 8))));
    (4, Join);
    (5, Stop);
  ];
  edges = [
    (0, None, None, 1);
    (1, None, None, 2);
    (2, Some No, None, 4);
    (2, Some Yes, None, 3);
    (3, None, None, 4);
    (4, None, Some [{
      assumption = True;
      conclusion = BoolExpr (LinExpr (1, "a", 0), Leq, Constant 42);
    }], 5);
  ];
}
