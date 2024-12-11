(* Advent of Code, day10 part1 and part2 - on BPI-F3 RISC-V SBC *)
(* OCaml code - *)

#load "utils.cmo"
open Utils

let char_to_int c = int_of_string (Char.escaped c)

let explode9 len s =
  List.init len (char_to_int << (String.get s))

let even n = n mod 2 = 0

let id x i = x

let rec file_map i j = function
    [] -> (0,[])
  | h::t -> if even i then match file_map (i+1) (j+1) t with
        (len,lst) -> let ls = List.init h (id j) in
        (List.length ls + len, ls @ lst)
    else match file_map (i+1) j t with
        (len,lst) -> let ls = List.init h (id (-1)) in
        (len, ls @ lst)

let rec right a i =
  if a.(i) = (-1) then i else right a (i+1)

let rec left a j =
  if a.(j) > (-1) then j else left a (j-1)
  
let rec compact1 a ii jj clen =
  if ii >= clen then a.(ii+1) <- (-2) else
    let (i,j) = (right a ii, left a jj) in
    begin a.(i) <- a.(j); compact1 a (i+1) (j-1) clen end

let rec checksum cs a i clen =
    if i >= clen then cs
    else if a.(i) < 0
    then checksum cs a (i+1) clen
    else checksum (a.(i) * i + cs) a (i+1) clen

let file_block a jj =
  if jj <= 0 then None else
    let j = ref jj in
    let rb = ref jj in
    let lb = ref jj in
    let n = ref (-1) in
    while !j > 0 && a.(!j) = (-1) do j := !j - 1 done;
    rb := !j; n := a.(!j);
    while !j > 0 && a.(!j) = !n do j := !j - 1 done;
    lb := !j+1; Some (!lb,!rb,!rb - !lb + 1)

let rec free_block a ii blen lb =
  if ii >= lb then None
  else 
    let i = ref ii in
    let lf = ref ii in
    let rf = ref ii in
    while !i < lb && a.(!i) > (-1) do i := !i + 1 done;
    lf := !i;
    while !i < lb && a.(!i) = (-1) do i := !i + 1 done;
    rf := !i-1;
    if (!rf - !lf + 1) < blen then free_block a (!rf+1) blen lb
    else Some (!lf, !rf)

let move a lf lb lnb =
  for i = lf to (lf+lnb-1) do a.(i) <- a.(lb-lf+i); a.(lb-lf+i) <- (-1)
  done
    
let rec compact2 a ii jj =
  match file_block a jj with
    None -> ()
  | Some (lb,rb,lnb) ->
    match free_block a ii lnb lb with
      None -> compact2 a 0 (lb-1)
    | Some (lf,rf) -> 
      move a lf lb lnb; compact2 a 0 (lb-1)
    
let day9 file =
  let raws = read_file file in
  let len = (String.length raws) - 1 in
  let chrlst = explode9 len raws in
  let (clen, lst) = file_map 0 0 chrlst in
  let arr1 = Array.of_list lst in
  let arr2 = Array.copy arr1 in
  let alen = Array.length arr1 - 1 in
  compact1 arr1 0 alen clen;
  compact2 arr2 0 alen;
  (checksum 0 arr1 0 clen, checksum 0 arr2 0 alen)

