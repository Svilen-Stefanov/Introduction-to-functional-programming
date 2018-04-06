(*Definitionen von den Typen*)
        (*type 'a list = [] | (::) of 'a * 'a list*)
        (*type unit = () *)
        (*eine unendliche Liste hat nur ein Konstruktor im Verleich zu den anderen*)
type 'a llist = Cons of 'a * (unit -> 'a llist);;

(*1. *)
let rec lseq x = Cons (x, fun () -> lseq (x+1))

(*Beispiel für die erste Funktion*)
let froml = lseq 1;;
let Cons (x, rest) = from1;;  (*Pattern matching auf der linken Seite*)
rest()
(**)

(*2. *)
let rec lconst x = Cons (x, fun () -> lconst (x))

(*3. *)
let lpowers2 () = 
    let rec lpowerHelp x = Cons (x, fun () -> lpowerHelp(2*x))
    in lpowerHelp 1;

(*4. *)
open Random
let rec lrng n = Cons(n, fun () -> lrng (Random.int n))

(*5. *)
let lfib () = 
    let rec lfibHelp x y = Cons (x, fun () -> lfibHelp y (x+y))
    in lfibHelp 0 1;

(*6. Erstes Element der Liste*)
let lhd (Cons(head, tail)) = head;

(*7. Tail der Liste zurückgeben*)
let ltl (Cons (_, tail)) = tail () 

(*8. Alle Elemente*) 
let rec ltake x (Cons (head, tail)) =
    if x = 0 then []
    else head :: (ltake (x-1) (tail ()))

(*Update version von Ocaml to 4.03.xx*)