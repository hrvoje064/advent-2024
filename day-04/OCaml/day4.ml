(* Advent of Code, day4 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

let find_xmas mx dx =
  let dir_l = ["u";"d";"l";"r";"nw";"ne";"sw";"se"] in
  let chr_l = ['M';'A';'S'] in

  let rec search i j iinc jinc dist =
    let ii,ji = (i + iinc * dist, j + jinc * dist) in
    if ii > dx || ii < 0 || ji > dx || ji < 0
    then 0 else
      let rec search i j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> search (i+iinc) (j+jinc) cs
        | _ -> 0
      in search (i+iinc) (j+jinc) chr_l
  in
  
  let rec search_all i j = function
      [] -> 0
    | "u"::dt -> search i j (-1) 0 3 + (search_all i j dt)
    | "d"::dt -> search i j 1 0 3 + (search_all i j dt)
    | "l"::dt -> search i j 0 (-1) 3 + (search_all i j dt)
    | "r"::dt -> search i j 0 1 3 + (search_all i j dt)
    | "nw"::dt -> search i j (-1) (-1) 3 + (search_all i j dt)
    | "ne"::dt -> search i j (-1) 1 3 + (search_all i j dt)
    | "sw"::dt -> search i j 1 (-1) 3 + (search_all i j dt)
    | _ -> search i j 1 1 3 + (search_all i j [])
  in
  
  let find =
    let matches = ref 0 in
    for i = 0 to dx do
      for j = 0 to dx do
        if mx.(i).(j) = 'X'
        then matches := search_all i j dir_l + !matches
      done
    done;
    !matches
  in find

let find_x_mas mx dx =
  let cha_l = ['M'; 'S'] in

  let nwse ii jj chr_l =
    if (jj-1) < 0 || (jj+1) > dx || (ii-1) < 0 || (ii+1) > dx then 0
    else let rec search i j jinc = function
          [] -> if jinc < 0 then 1 else search (ii-1) (jj+1) (-2) cha_l
        | [a;b] when mx.(i).(j) = a -> search (i+2) (j+jinc) jinc [b]
        | [a;b] when mx.(i).(j) = b -> search (i+2) (j+jinc) jinc [a]
        | [a] when mx.(i).(j) = a -> search i j jinc []
        | _ -> 0
      in search (ii-1) (jj-1) 2 chr_l in

  let find =
    let matches = ref 0 in
    for i = 0 to dx do
      for j = 0 to dx do
        if mx.(i).(j) = 'A'
        then matches := nwse i j cha_l + !matches
      done
    done;
    !matches
  in find

let day4 file =
  let rawl = read_lines file in
  let dx = String.length (List.hd rawl) in
  let mx = Array.make_matrix dx dx '.' in
  List.iteri (fun i str -> String.iteri
                 (fun j c -> mx.(i).(j) <- c) str) rawl;
  (find_xmas mx (dx - 1), find_x_mas mx (dx - 1))

