(* Advent of Code - 2024; day1 - part2 on BPI-F3 RISC-V *)
(* OCaml much faster code using BST instead of filter *)

#load "utils.cmo"
open Utils

let rec unleave = function
    [] -> [],[]
  | [a; b] :: t -> match unleave t with
      l, r -> (int_of_string a :: l, int_of_string b :: r)

let day_1b file =
  let ll,rl = file |> read_lol_words |> unleave in
  let bst = bst_of_list rl in
  List.(fold_left (+) 0 (map (fun l -> l * (get l bst)) ll))
