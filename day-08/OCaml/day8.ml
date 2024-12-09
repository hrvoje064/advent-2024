(* Advent of Code, day8 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

let do_day8 gs grid antsh =

  let find_all_ant () =
  for i = 0 to gs do
    for j = 0 to gs do
      let c = grid.(i).(j) in
      if c <> '.' then Hashtbl.add antsh c (i,j);
    done
  done
  in
  
  let is_in (x,y) = x >= 0 && x <= gs && y >= 0 && y <= gs
  in

  let rec antinodes nodes calc_f = function
      [] -> nodes
    | c::t ->
      antinodes
        (calc_f (combinations (Hashtbl.find_all antsh c) ~size:2) @ nodes)
        calc_f t
  in

  let part1 antl =
    
    let rec calc = function
        [] -> []
      | [(x1,y1);(x2,y2)]::t ->
        let (dx,dy) = (x2-x1, y2-y1) in
        let (p1,p2) = (x1-dx,y1-dy),(x2+dx,y2+dy) in
        p1 :: p2 :: (calc t)
      | _ -> raise (Invalid_argument "calc")
    in
    List.length (List.sort_uniq compare
                   (List.filter is_in (antinodes [] calc antl)))

  in

  let part2 antl =

    let rec res_l acc dx dy x y =
      let (x1,y1) = (x-dx,y-dy) in
      if x1 < 0 || x1 > gs || y1 < 0 || y1 > gs
      then acc else res_l ((x1,y1)::acc) dx dy x1 y1
    in
    let rec res_r acc dx dy x y =
      let (x1,y1) = (x+dx,y+dy) in
      if x1 < 0 || x1 > gs || y1 < 0 || y1 > gs
      then acc else res_r ((x1,y1)::acc) dx dy x1 y1
    in

    let resonate acc dx dy x1 y1 x2 y2 =
      res_r (res_l acc dx dy x1 y1) dx dy x2 y2
    in
  
    let rec calc2 = function
        [] -> []
      | [(x1,y1);(x2,y2)]::t ->
        let (dx,dy) = (x2-x1, y2-y1) in
        resonate [(x1,y1);(x2,y2)] dx dy x1 y1 x2 y2 @ (calc2 t)
      | _ -> raise (Invalid_argument "calc2")
    in
    List.length (List.sort_uniq compare
                   (List.filter is_in (antinodes [] calc2 antl)))

  in
  find_all_ant ();
  let antl = List.sort_uniq compare
      (List.of_seq (Hashtbl.to_seq_keys antsh))
  in
  (part1 antl, part2 antl)
  
let day8 file =
  let rawl = read_lines file in
  let gs = String.length (List.hd rawl) in
  let grid = Array.make_matrix gs gs '.' in
  List.iteri (fun j str ->
      String.iteri (fun i c -> grid.(i).(j) <- c) str) rawl;
  let antsh = Hashtbl.create 64 in
  let result = do_day8 (gs-1) grid antsh in
  result

  



