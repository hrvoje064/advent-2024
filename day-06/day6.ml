(* Advent of Code, day6 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code *)

#load "utils.cmo"
open Utils

exception Not_found
  
let find_guard dx mx =
  let rec find i j =
    match i,j with
      i,_ when i >= dx -> find 0 (j+1)
    | _,j when j >= dx -> raise Not_found
    | i,j when mx.(i).(j) = '^' -> (i,j)
    | _ -> find (i+1) j
  in find 0 0

let guard_path acc i j dx dl mx =
  let rec path acc i j dl =
    match dl with
    'N'::dt -> if i = 0 then acc
      else if mx.(i-1).(j) = '#'
      then path acc i j (dt @ ['N'])
      else path (((i-1),j)::acc) (i-1) j dl
    | 'E'::dt -> if j = dx then acc
      else if mx.(i).(j+1) = '#'
      then path acc i j (dt @ ['E'])
      else path ((i,(j+1))::acc) i (j+1) dl
    | 'S'::dt -> if i = dx then acc
      else if mx.(i+1).(j) = '#'
      then path acc i j (dt @ ['S'])
      else path (((i+1),j)::acc) (i+1) j dl
    | 'W'::dt -> if j = 0 then acc
      else if mx.(i).(j-1) = '#'
      then path acc i j (dt @ ['W'])
      else path ((i,(j-1))::acc) i (j-1) dl
    | _ -> raise Not_found
  in List.sort_uniq compare (path acc i j dl)

let is_loop gi gj dx dl mx =
  let newp = Hashtbl.create 256 in
  let rec path i j dl =
    match dl with
    |'N'::_ when Hashtbl.mem newp (i,j,'N') -> true 
    |'N'::dt -> if i = 0 then false
      else if mx.(i-1).(j) = '#'
      then begin (Hashtbl.add newp (i,j,'N') true); path i j (dt @ ['N']) end
      else path (i-1) j dl
    |'E'::_ when Hashtbl.mem newp (i,j,'E') -> true
    |'E'::dt -> if j = dx then false
      else if mx.(i).(j+1) = '#'
      then begin (Hashtbl.add newp (i,j,'E') true); path i j (dt @ ['E']) end
      else path i (j+1) dl
    |'S'::_ when Hashtbl.mem newp (i,j,'S') -> true
    |'S'::dt -> if i = dx then false
      else if mx.(i+1).(j) = '#'
      then begin (Hashtbl.add newp (i,j,'S') true); path i j (dt @ ['S']) end
      else path (i+1) j dl
    |'W'::_ when Hashtbl.mem newp (i,j,'W') -> true
    |'W'::dt -> if j = 0 then false
      else if mx.(i).(j-1) = '#'
      then begin (Hashtbl.add newp (i,j,'W') true); path i j (dt @ ['W']) end
      else path i (j-1) dl
    | _ -> raise Not_found
  in path gi gj dl

let obstructions gi gj dx dl path mx =
  let rec put_obs acc = function
      [] -> acc
    | (i,j)::ps -> mx.(i).(j) <- '#';
      if is_loop gi gj dx dl mx
      then begin mx.(i).(j) <- 'X'; put_obs (acc+1) ps end
      else begin mx.(i).(j) <- 'X'; put_obs acc ps end
  in put_obs 0 path

let day6 file =
  let dirl = ['N';'E';'S';'W'] in
  let rawl = read_lines file in
  let dx = String.length (List.hd rawl) in
  let mx = Array.make_matrix dx dx '.' in
  List.iteri (fun i str -> String.iteri
                 (fun j c -> mx.(i).(j) <- c) str) rawl;
  let (i,j) = find_guard dx mx in
  mx.(i).(j) <- 'X';
  let path = guard_path [(i,j)] i j (dx-1) dirl mx in
  let obs = obstructions i j (dx-1) dirl path mx in
  (List.length path, obs)
