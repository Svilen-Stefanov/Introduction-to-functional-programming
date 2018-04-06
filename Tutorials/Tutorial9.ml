(*
(-^_^-) Theorie: (-^_^-)

val ref: 'a -> 'a ref
val (!) : 'a ref-> 'a
val ( := ) : 'a ref-> 'a -> unit

for <var> = <first> to <last> do
  <body>
done

*)

(*Aufgabe 9.1*)
(*Batteries packet*)
let reverse_line file_name =
  let input = open_in file_name
  in let rec read_lines_rev acc =
    try read_lines_rev(input_line input :: acc) with
      End_of_file -> close_in input; acc
  in let reversed = read_lines_rev []
  in let output = open_out file_name    (*muss nur einmal gemacht werden, nachdem man alles richtig gelesen hat*)
  (*in let reverse_line l = String.concat " " (List.rev (String.split_on_char ' ' l))*)
  in let reverse_line l = String.split_on_char ' ' l |> List.rev |> String.concat " " (*Equivallent to the upper one*)
  in List.rev reversed
    |> List.map reverse_line  (*Nimmt eine Funktion und eine Liste und wendet die funktion an der Argument*)
    |> List.iter (output_string output); (* = fun l -> output_string output l*)
    close_out output

(*Aufgabe 9.2*)
let decreasing var = let l = ref [] in
  for i = 0 to var do
    l := i :: !l
  done;
  !l

  (*
  !!Exceptions!!
  try <exep> with
    <pattern1> -> <expr1>
    |<pattern2> -> <expr2>


    EXAMPLE:
    exception Empty_option

    let get_option = function
      Some x -> x
      |None -> raise Empty_option

    let get_option_default opt def =
      try get_option opt with
        Empty_option -> def
  *)


(*Aufgabe 9.3*)
exception Wrong_value

let apply_even_pos f i =
    if(i mod 2 = 0 && i > 0) then f i
    else raise Wrong_value

let apply_f_print_error f i =
  try apply_even_pos f i with
    Wrong_value -> Printf.printf ("Error!\n")


    exception Negative_argument
    exception Zero_argument
    exception Odd_argument

    let apply_even_pos (f : int->int) n =
      if n < 0 then
        raise Negative_argument
      else if n = 0 then
        raise Zero_argument
      else if n mod 2 != 0 then
        raise Odd_argument else
        f n

    let apply_f_print_error f n =
      try Printf.printf "Result: %d\n" (apply_even_pos f n) with
        Negative_argument -> Printf.printf "Argument is negative!\n"
        | Zero_argument -> Printf.printf "Argument is zero .-(\n"
        | Odd_argument -> Printf.printf "Argument is odd\n"

        let apply_f_print_error f n =
          try Printf.printf "Result: %d\n" (apply_even_pos f n) with
            Negative_argument -> Printf.printf "Argument is negative!\n"
            | Zero_argument -> Printf.printf "Argument is zero .-(\n"
            | Odd_argument -> Printf.printf "Argument is odd\n"
