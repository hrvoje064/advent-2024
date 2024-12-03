(* Advent of Code, day2 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils




let day2 file =
  let s = read_file file in
  let sl = List.map (String.split_on_char ')')
      (String.split_on_char '(' s) in
  let mul_nums =
    List.(map
            (fun l -> map (fun s ->
                 let len = String.length s in
                 if len < 3 then ""
                 else 
                              
      


  
