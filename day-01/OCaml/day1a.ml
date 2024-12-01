(* Advent of Code - 2024; day1 - part1 on BPI-F3 RISC-V SBC *)

#load "utils.cmo"
open Utils

(* OCaml code *)

let rec unleave = function
    [] -> [],[]
  | [a; b] :: t -> match unleave t with
      l, r -> (int_of_string a :: l, int_of_string b :: r)

let day_1a file =
  let ll,rl = file |> read_lol_words |> unleave in
  let sll = List.sort compare ll in
  let srl = List.sort compare rl in
  List.(fold_left (+) 0 (map2 (fun l r -> abs (l - r)) sll srl))

(* run in utop:

   #use "day1a.ml";;

   time (fun () -> day_1a "input-1a.txt");;

   Execution time: 0.012914s

   - : int = 3508942
*)
