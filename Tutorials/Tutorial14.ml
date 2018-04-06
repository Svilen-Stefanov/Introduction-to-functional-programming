(*Aufgabe 14.1 - Nebenläufige Kamele*)
(*
Thread.create : ('a -> 'b) -> 'a -> Thread.t

Kommunikation ('a Event.channel):
Event.new_channel () (*neuen Kanal erzeugen*)
Event.sync (Event.receive c) (*ein Event c zurückbekommen vom Kanal*)
Event.sync (Event.send c x) (* Wert x über Kanal c schicken*)
*)

(*zum kompilieren:
#require "threads.posix"
ocamlc -thread unix.cma threads.cma file.ml 

open Sys
open Unix
open Thread
open Event
*)

(* List.rev(List.rev l1) == List.(rev (rev l1)) *)

module Server : sig
    type ('a, 'b) t
    val serve : ('a -> 'b) -> ('a, 'b) t
end = struct 
    type ('a, 'b) t = ('a * 'b Event.channel) Event.channel
    let serve f = 
        let c = Event.new_channel ()
        in let rec task () = 
            let (a, r) = Event.(sync (receive c))
            in let _ = Thread.create (fun () -> Event.(sync (send r (f a)))) ()
            in task ()
        in let _ = Thread.create task ()
        in c

    let request s a = 
        let c = Event.new_channel ()
        in Event.(sync (send s (a, c))); (*statt 2 mal Event zu schreiben*)
        Event.(sync (receive c))

(*Aufgabe 14.2 - Vektor-Matrix-Multiplikation*)

type col = int list
[@@deriving show]

type matrix = col list
[@@deriving show]

(* transposer *)
let transpose (miso, matrix) =
  let rec foreach_column rest =
    (* transpose *)
    let (column, rest) = List.fold_right (fun x (c, r) ->
        match x with
          hd::tl -> (hd::c, tl::r)
        | _ -> ([], [])
      ) rest ([], []) in
    (* send result *)
    sync (send miso (Some column));
    (* continue if not finished *)
    match rest with
      hd::tl -> (match hd with
          [] -> ()
        | _ -> foreach_column rest)
    | _ -> assert false in
  foreach_column matrix;
  sync (send miso None)

(* multiplier *)
let multiply (v, miso, mosi) =
  let rec inner () =
    (* receive transposed line *)
    let column = (sync (receive mosi)) in
    match column with
      Some column ->
      (* multiply *)
      let multiplied = List.map2 (fun x y -> x*y) v column in
      (* send result to master *)
      sync (send miso multiplied);
      (* continue *)
      inner ()
    (* Got a None? Finished! *)
    | _ -> () in
  inner ()

(* summer *)
let sum (miso, mosi) =
  let rec inner () =
    (* receive transposed and multiplied line *)
    let vec = (sync (receive mosi)) in
    match vec with
      Some vec ->
      (* sum *)
      let sum = List.fold_left (+) 0 vec in
      (* send result to master *)
      sync (send miso sum);
      (* continue *)
      inner ()
    (* Got a None? Finished! *)
    | _ -> () in
  inner ()
  
type worker = Multiplier | Summer

type state = {
    (*transposed columns*)
    trd :  col list;
    multd : col list;
    result : int list;
    free : worker list;
    trans_finished : bool;
}

let start_state = {
    trd = [];
    multd = [];
    result = [];
    free = [Multiplier;Summer];
    trans_finished = false;
}

let multiply vector matrix =
  (* miso: master in / slave out, mosi: master out, slave in *)
  let transpose_miso = new_channel () in
  (* different types require different channel *)
  let (mul_miso, mul_mosi) = (new_channel (), new_channel ()) in
  let (sum_miso, sum_mosi) = (new_channel (), new_channel ()) in
  let transposer = Thread.create transpose (transpose_miso, matrix) in 
  let multiplier = Thread.create multiply (vector, mul_miso, mul_mosi) in
  let summer = Thread.create sum (sum_miso, sum_mosi) in 
  let rec loop old_state = 
      let state = List.fold_left (fun x worker -> 
      match worker, s with 
        Multiplier, (trd = thd :: ttl) -> Event.(sync (send mul_mosi (Some thd))); {s with trd = ttl}
        | Summer , (multd = mulhd :: multl) -> Event.(sync (send sum_mosi (Some mulhd))); {s with multd = multl}
        |_ -> {s with free = worker::s.free}
      ) {old_state with free = []} old_state.free
      in if state.trans_fiiinished && List.length old_state.free = 2
        then state
        else loop Event.(select [
            wrap (receive transpose_miso) (function 
            | None -> {state with trans_finished = true}
            | Some col -> {state with trd = state.trd @ [col]}); 

            wrap (receive mul_miso) (fun vec ->
            {state with multd = state.multd @ [vec];
            free = Multiplier :: state.free});

            wrap (receive sum_miso) (fun v ->
            {state with multd = state.result @ [v];
            free = Summer :: state.free})
        ]) 
  in let state = loop start_state
  in Event.(sync (send mul_mosi None ));
  Event.(sync (send sum_mosi None));
  List.iter Thread.join [transposerl; multiplier; summer];
  state.result
  


let () =
  let matrix = [
    [1; 2; 3];
    [4; 5; 6]
  ] in
  let vector = [2; 3] in
  ()