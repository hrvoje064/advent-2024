(* Advent of Code, day7 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

let calculate1 result nums =
  let rec calc = function
      [rr] -> rr = result 
    | rr::a::t -> calc ((rr + a) :: t) || calc ((rr * a) :: t)
  in calc nums

let cat a b =
  int_of_string (string_of_int a ^ (string_of_int b))

let calculate2 result nums =
  let rec calc = function
      [rr] -> rr = result 
    | rr::a::t -> calc ((rr + a) :: t) ||
                  calc ((rr * a) :: t) ||
                  calc ((cat rr a) :: t)
  in calc nums

let day7 file =
  let rawl = read_lines file in
  let nums =
    List.map ((List.map int_of_string) << (string_split [':'; ' '])) rawl in
  let r_tups = List.map (fun l -> (List.hd l, List.tl l)) nums in
  let all_terms = List.map
      (fun (r,ns) -> if calculate1 r ns then (r,[]) else (r,ns)) r_tups in
  let (ok_terms,wr_terms) = filter_split
      (fun x -> match x with (_,[]) -> true | _ -> false) all_terms in
  let cat_terms = List.map
      (fun (r,ns) -> if calculate2 r ns then r else 0) wr_terms in
  let (a,b) = (List.(fold_left (+) 0 (map fst ok_terms)),
               List.fold_left (+) 0 cat_terms) in
  (a, a + b)
              
