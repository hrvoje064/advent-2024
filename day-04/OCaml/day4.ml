(* Advent of Code, day4 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

let find_xmas mx dx =
  let dir_l = ["u";"d";"l";"r";"nw";"ne";"sw";"se"] in
  let chr_l = ['M';'A';'S'] in

  let up i j =
    if (i-2) < 0 then 0
    else
      let rec up i = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> up (i-1) cs
        | _ -> 0
      in up i chr_l in
  let dn i j =
    if (i+2) > dx then 0
    else
      let rec dn i = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> dn (i+1) cs
        | _ -> 0
      in dn i chr_l in
  let lt i j =
    if (j-2) < 0 then 0
    else
      let rec lt j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> lt (j-1) cs
        | _ -> 0
      in lt j chr_l in
  let rt i j =
    if (j+2) > dx then 0
    else
      let rec rt j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> rt (j+1) cs
        | _ -> 0
      in rt j chr_l in
  let nw i j =
    if (j-2) < 0 || (i-2) < 0 then 0
    else
      let rec nw i j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> nw (i-1) (j-1) cs
        | _ -> 0
      in nw i j chr_l in
  let ne i j =
    if (j+2) > dx || (i-2) < 0 then 0
    else
      let rec ne i j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> ne (i-1) (j+1) cs
        | _ -> 0
      in ne i j chr_l in
  let sw i j =
    if (j-2) < 0 || (i+2) > dx then 0
    else
      let rec sw i j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> sw (i+1) (j-1) cs
        | _ -> 0
      in sw i j chr_l in
  let se i j =
    if (j+2) > dx || (i+2) > dx then 0
    else
      let rec se i j = function
          [] -> 1
        | c::cs when c = mx.(i).(j) -> se (i+1) (j+1) cs
        | _ -> 0
      in se i j chr_l in
  
  let rec search_all i j = function
      [] -> 0
    | "u"::dt -> up (i-1) j + (search_all i j dt)
    | "d"::dt -> dn (i+1) j + (search_all i j dt)
    | "l"::dt -> lt i (j-1) + (search_all i j dt)
    | "r"::dt -> rt i (j+1) + (search_all i j dt)
    | "nw"::dt -> nw (i-1) (j-1) + (search_all i j dt)
    | "ne"::dt -> ne (i-1) (j+1) + (search_all i j dt)
    | "sw"::dt -> sw (i+1) (j-1) + (search_all i j dt)
    | _ -> se (i+1) (j+1) + (search_all i j [])
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

  let sw i j c=
    if mx.(i).(j) = c then 1 else 0 in 
  
  let nesw i j =
    if j > dx || (j-2) < 0 || i < 0 || (i+2) > dx then 0
    else if mx.(i).(j) = 'M' then sw (i+2) (j-2) 'S'
    else if mx.(i).(j) = 'S' then sw (i+2) (j-2) 'M'
    else 0 in

  let se i j c =
    if mx.(i).(j) = c then nesw (i-2) j else 0 in

  let nwse i j =
    if j < 0 || (j+2) > dx || i < 0 || (i+2) > dx then 0
    else if mx.(i).(j) = 'M' then se (i+2) (j+2) 'S'
    else if mx.(i).(j) = 'S' then se (i+2) (j+2) 'M'
    else 0 in

  let find =
    let matches = ref 0 in
    for i = 0 to dx do
      for j = 0 to dx do
        if mx.(i).(j) = 'A'
        then matches := nwse (i-1) (j-1) + !matches
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
