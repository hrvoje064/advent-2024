(* Advent of Code, day2 part1 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

let rec safe_up = function
    [_] -> true
  | x::y::t -> x < y && (y - x) <= 3 && safe_up (y::t)
  | _ -> false

let rec safe_down = function
    [_] -> true
  | x::y::t -> x > y && (x - y) <= 3 && safe_down (y::t)
  | _ -> false

let safe = function
    (x::y::t as l) -> if x < y then safe_up l else safe_down l
  | _ -> false

let day2a file =
  let ll = read_process_lines
      ((List.map int_of_string) << (String.split_on_char ' ')) file in
  List.(length (filter safe ll))
  
